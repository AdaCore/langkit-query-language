//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.checker.built_ins;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.annotations.BuiltInFunction;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.checker.NodeChecker;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBoolean;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBooleanNodeGen;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.LangkitLocationWrapper;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.adacore.lkql_jit.values.LKQLFunction;
import com.adacore.lkql_jit.values.LKQLUnit;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;
import java.util.LinkedList;

public final class NodeCheckerFunction {

    private static final LKQLToBoolean toBoolean = LKQLToBooleanNodeGen.create();

    public record InstantiatedNodeChecker(
        NodeChecker checker,
        LKQLFunction function,
        Object[] arguments
    ) {}

    public static InstantiatedNodeChecker[] preprocessNodeCheckers(
        VirtualFrame frame,
        NodeChecker[] checkers,
        LKQLContext context
    ) {
        var instantiatedFuncs = new InstantiatedNodeChecker[checkers.length];
        for (int i = 0; i < checkers.length; i++) {
            var checker = checkers[i];
            var function = checker.getFunction();
            var aliasName = checker.getAlias();
            var lowerRuleName = StringUtils.toLowerCase(checker.getName());
            var instance = new InstantiatedNodeChecker(
                checker,
                function,
                new Object[function.parameterNames.length + 1]
            );

            for (int j = 1; j < function.getParameterDefaultValues().length; j++) {
                String paramName = function.parameterNames[j];
                Object userDefinedArg = context.getRuleArg(
                    (aliasName == null ? lowerRuleName : StringUtils.toLowerCase(aliasName)),
                    StringUtils.toLowerCase(paramName)
                );
                instance.arguments[j + 1] = userDefinedArg == null
                    ? ((Expr) function.getParameterDefaultValues()[j]).executeGeneric(frame)
                    : userDefinedArg;
            }

            instantiatedFuncs[i] = instance;
        }
        return instantiatedFuncs;
    }

    /**
     * This class is the expression of the "node_checker" built-in. This expression contains the
     * traversing logic to check the nodes.
     */
    @BuiltInFunction(
        name = "node_checker",
        doc = "Given a root, execute all node checkers while traversing the tree"
    )
    abstract static class NodeCheckerExpr extends BuiltInBody {

        private final InteropLibrary interopLibrary = InteropLibrary.getUncached();

        @Specialization
        public Object alwaysTrue(VirtualFrame frame, LangkitSupport.NodeInterface root) {
            return implem(frame.materialize(), root);
        }

        @CompilerDirectives.TruffleBoundary
        public Object implem(MaterializedFrame frame, LangkitSupport.NodeInterface root) {
            // Get the arguments
            final LKQLContext context = LKQLLanguage.getContext(this);
            final LangkitSupport.AnalysisUnit rootUnit = root.getUnit();

            final InstantiatedNodeChecker[] allNodeCheckers = preprocessNodeCheckers(
                frame,
                context.getAllNodeCheckers(),
                context
            );
            final InstantiatedNodeChecker[] nodeCheckers = preprocessNodeCheckers(
                frame,
                context.getNodeCheckers(),
                context
            );
            final InstantiatedNodeChecker[] sparkNodeCheckers = preprocessNodeCheckers(
                frame,
                context.getSparkNodeCheckers(),
                context
            );

            final boolean mustFollowInstantiations = context.mustFollowInstantiations();
            final boolean hasSparkCheckers = sparkNodeCheckers.length > 0;

            // Traverse the tree
            // Create the list of node to explore with the generic instantiation info
            final LinkedList<VisitStep> visitList = new LinkedList<>();
            visitList.add(new VisitStep(root, false, false));

            // Iterate over all nodes of the tree
            while (!visitList.isEmpty()) {
                // Get the current values
                final VisitStep currentStep = visitList.remove(0);
                final LangkitSupport.NodeInterface currentNode = currentStep.node();
                final boolean inGenericInstantiation = currentStep.inGenericInstantiation();
                final boolean inSparkCode = currentStep.inSparkCode();

                try {
                    if (
                        mustFollowInstantiations &&
                        currentNode instanceof Libadalang.GenericInstantiation genInst
                    ) {
                        // If the node is a generic instantiation, traverse the instantiated generic
                        final Libadalang.BasicDecl genDecl = genInst.pDesignatedGenericDecl();
                        final Libadalang.BodyNode genBody = genDecl.pBodyPartForDecl(false);

                        if (!genBody.isNone()) {
                            visitList.addFirst(new VisitStep(genBody, true, inSparkCode));
                        }
                        visitList.addFirst(new VisitStep(genDecl, true, inSparkCode));
                    } else if (
                        inGenericInstantiation && currentNode instanceof Libadalang.BodyStub stub
                    ) {
                        // If this node is a body stub and we are currently traversing a generic
                        // instantiation,
                        // we should also traverse the stub's completion.
                        final Libadalang.BasicDecl stubBody = stub.pNextPartForDecl(false);
                        visitList.addFirst(new VisitStep(stubBody, true, inSparkCode));
                    }
                } catch (Libadalang.LangkitException e) {
                    // Get the stack frame below this one to get a location for the error
                    var stackTrace = TruffleStackTrace.getStackTrace(e);
                    var stackFrame = stackTrace.get(1);
                    var callerLocation = getClosestNodeWithSourceInfo(stackFrame.getLocation());

                    context
                        .getDiagnosticEmitter()
                        .emitDiagnostic(
                            CheckerUtils.MessageKind.ERROR,
                            e.getMessage(),
                            new LangkitLocationWrapper(currentNode, context.linesCache),
                            new SourceSectionWrapper(callerLocation.getSourceSection())
                        );
                    if (context.isCheckerDebug()) {
                        context.getLogger().severe(e.getMessage());
                        context.getLogger().severe(e.nativeStackTrace());
                    }
                    continue;
                }

                // Apply the "both" checkers
                this.executeCheckers(frame, currentStep, currentNode, allNodeCheckers, context);

                // If we're in Ada code execute the Ada checkers else execute the SPARK checkers
                if (inSparkCode) {
                    this.executeCheckers(
                        frame,
                        currentStep,
                        currentNode,
                        sparkNodeCheckers,
                        context
                    );
                } else {
                    this.executeCheckers(frame, currentStep, currentNode, nodeCheckers, context);
                }

                // Add the children to the visit list
                for (int i = currentNode.getChildrenCount() - 1; i >= 0; i--) {
                    final LangkitSupport.NodeInterface child = currentNode.getChild(i);
                    if (!child.isNone()) {
                        // No need to check if the child is a base subprogram
                        // body in SPARK mode if there is no required SPARK
                        // checkers. This avoids useless calls to
                        // 'pIsSubjectToProof'.
                        if (hasSparkCheckers && child instanceof Libadalang.BaseSubpBody subpBody) {
                            visitList.addFirst(
                                new VisitStep(
                                    child,
                                    inGenericInstantiation,
                                    subpBody.pIsSubjectToProof()
                                )
                            );
                        } else {
                            visitList.addFirst(
                                new VisitStep(child, inGenericInstantiation, inSparkCode)
                            );
                        }
                    }
                }
            }

            // Return the unit instance
            return LKQLUnit.INSTANCE;
        }

        /**
         * Execute the given checker array to the given Ada node.
         */
        private void executeCheckers(
            VirtualFrame frame,
            VisitStep currentStep,
            LangkitSupport.NodeInterface currentNode,
            InstantiatedNodeChecker[] checkers,
            LKQLContext context
        ) {
            // For each checker apply it on the current node if needed
            for (var checker : checkers) {
                if (
                    !currentStep.inGenericInstantiation() ||
                    checker.checker.isFollowGenericInstantiations()
                ) {
                    try {
                        this.applyNodeRule(frame, checker, currentNode, context);
                    } catch (LKQLRuntimeError e) {
                        if (e.getCause() != null) {
                            if (context.isCheckerDebug()) {
                                context
                                    .getDiagnosticEmitter()
                                    .emitDiagnostic(
                                        CheckerUtils.MessageKind.ERROR,
                                        e.getMessage(),
                                        new LangkitLocationWrapper(currentNode, context.linesCache),
                                        e.getLocation() == null
                                            ? null
                                            : new SourceSectionWrapper(
                                                  e.getLocation().getSourceSection()
                                              ),
                                        checker.checker.getName()
                                    );
                            }
                        } else {
                            context
                                .getDiagnosticEmitter()
                                .emitDiagnostic(
                                    CheckerUtils.MessageKind.ERROR,
                                    e.getMessage(),
                                    new LangkitLocationWrapper(currentNode, context.linesCache),
                                    new SourceSectionWrapper(e.getLocation().getSourceSection()),
                                    checker.checker.getName()
                                );
                        }
                    }
                }
            }
        }

        /**
         * Apply the checker on the given node.
         */
        private void applyNodeRule(
            VirtualFrame frame,
            InstantiatedNodeChecker checker,
            LangkitSupport.NodeInterface node,
            LKQLContext context
        ) {
            // Place the closure in the arguments
            checker.arguments[0] = checker.function.closure;
            checker.arguments[1] = node;

            // Call the checker
            final boolean ruleResult;
            try {
                ruleResult = toBoolean.execute(
                    interopLibrary.execute(checker.function, checker.arguments)
                );
            } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
                // TODO: Move function runtime verification to the LKQLFunction class (#138)
                throw LKQLEngineException.create(e);
            }

            if (ruleResult) {
                reportViolation(context, checker.checker, node);
            }
        }

        /**
         * Report a rule violation with the node that violate it.
         *
         * @param context The context to output the message.
         * @param checker The checker corresponding to the violated rule.
         * @param node The node that violated the checker.
         */
        @CompilerDirectives.TruffleBoundary
        private static void reportViolation(
            LKQLContext context,
            NodeChecker checker,
            LangkitSupport.NodeInterface node
        ) {
            if (node instanceof Libadalang.BasicDecl basicDecl) {
                LangkitSupport.NodeInterface definingName = basicDecl.pDefiningName();
                node = definingName.isNone() ? node : definingName;
            }
            context
                .getDiagnosticEmitter()
                .emitRuleViolation(
                    checker,
                    checker.getMessage(),
                    new LangkitLocationWrapper(node, context.linesCache),
                    // TODO: Genericize LKQL issue #500. Cannot interface Ada specific calls.
                    ((Libadalang.AdaNode) node).pGenericInstantiations(),
                    context
                );
        }

        /** Get the closest parent node with a location from the provided node. */
        private static Node getClosestNodeWithSourceInfo(Node node) {
            while (node != null && node.getSourceSection() == null) {
                node = node.getParent();
            }
            return node;
        }

        // ----- Inner classes -----

        /**
         * This record contains the information for a visiting step.
         *
         * @param node The node to visit.
         * @param inGenericInstantiation Whether the visit is currently in a generic instantiation.
         */
        private record VisitStep(
            LangkitSupport.NodeInterface node,
            boolean inGenericInstantiation,
            boolean inSparkCode
        ) {}
    }
}
