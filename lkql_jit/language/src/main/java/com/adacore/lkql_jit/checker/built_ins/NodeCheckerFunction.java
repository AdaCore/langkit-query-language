//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.checker.built_ins;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.checker.NodeChecker;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.LalLocationWrapper;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.util.LinkedList;

/**
 * This class represents the "node_checker" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NodeCheckerFunction {

    // ----- Attributes -----

    /** The name of the built-in. */
    public static final String NAME = "node_checker";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a root, execute all node checker while traverse the tree",
                new String[] {"root"},
                new Expr[] {null},
                new NodeCheckerExpr());
    }

    // ----- Inner classes -----

    /**
     * This class is the expression of the "node_checker" built-in. This expression contains the
     * traversing logic to checker the nodes.
     */
    private static final class NodeCheckerExpr extends AbstractBuiltInFunctionBody {

        /** An uncached interop library for the checker functions execution. */
        private InteropLibrary interopLibrary = InteropLibrary.getUncached();

        /**
         * @see
         *     AbstractBuiltInFunctionBody#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
         */
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            final LKQLContext context = LKQLLanguage.getContext(this);
            final Libadalang.AdaNode root;
            final NodeChecker[] allNodeCheckers = context.getAllNodeCheckers();
            final NodeChecker[] adaNodeCheckers = context.getAdaNodeCheckers();
            final NodeChecker[] sparkNodeCheckers = context.getSparkNodeCheckers();
            final boolean mustFollowInstantiations = context.mustFollowInstantiations();
            final boolean hasSparkCheckers = sparkNodeCheckers.length > 0;

            try {
                root = LKQLTypeSystemGen.expectAdaNode(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ADA_NODE,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[0]);
            }

            // Traverse the tree
            // Create the list of node to explore with the generic instantiation info
            final LinkedList<VisitStep> visitList = new LinkedList<>();
            visitList.add(new VisitStep(root, false, false));

            // Iterate over all nodes of the tree
            while (!visitList.isEmpty()) {
                // Get the current values
                final VisitStep currentStep = visitList.remove(0);
                final Libadalang.AdaNode currentNode = currentStep.node();
                final boolean inGenericInstantiation = currentStep.inGenericInstantiation();
                final boolean inSparkCode = currentStep.inSparkCode();

                try {
                    if (mustFollowInstantiations
                            && currentNode instanceof Libadalang.GenericInstantiation genInst) {
                        // If the node is a generic instantiation, traverse the instantiated generic
                        final Libadalang.BasicDecl genDecl = genInst.pDesignatedGenericDecl();
                        final Libadalang.BodyNode genBody = genDecl.pBodyPartForDecl(false);

                        if (!genBody.isNone()) {
                            visitList.addFirst(new VisitStep(genBody, true, inSparkCode));
                        }
                        visitList.addFirst(new VisitStep(genDecl, true, inSparkCode));
                    } else if (inGenericInstantiation
                            && currentNode instanceof Libadalang.BodyStub stub) {
                        // If this node is a body stub and we are currently traversing a generic
                        // instantiation,
                        // we should also traverse the stub's completion.
                        final Libadalang.BasicDecl stubBody = stub.pNextPartForDecl(false);
                        visitList.addFirst(new VisitStep(stubBody, true, inSparkCode));
                    }
                } catch (Libadalang.LangkitException e) {
                    context.println(
                            StringUtils.concat(
                                    "Error during generic instantiation walking: ",
                                    e.getMessage()));
                    continue;
                }

                // Apply the "both" checkers
                this.executeCheckers(frame, currentStep, currentNode, allNodeCheckers, context);

                // If we're in Ada code execute the Ada checkers else execute the SPARK checkers
                if (inSparkCode) {
                    this.executeCheckers(
                            frame, currentStep, currentNode, sparkNodeCheckers, context);
                } else {
                    this.executeCheckers(frame, currentStep, currentNode, adaNodeCheckers, context);
                }

                // Add the children to the visit list
                for (int i = currentNode.getChildrenCount() - 1; i >= 0; i--) {
                    final Libadalang.AdaNode child = currentNode.getChild(i);
                    if (!child.isNone()) {
                        // No need to check if the child is a base subprogram body in SPARK mode if
                        // there is no
                        // required
                        // SPARK checkers. This avoids useless calls to 'pIsSubjectToProof'.
                        if (hasSparkCheckers && child instanceof Libadalang.BaseSubpBody subpBody) {
                            visitList.addFirst(
                                    new VisitStep(
                                            child,
                                            inGenericInstantiation,
                                            subpBody.pIsSubjectToProof()));
                        } else {
                            visitList.addFirst(
                                    new VisitStep(child, inGenericInstantiation, inSparkCode));
                        }
                    }
                }
            }

            // Return the unit instance
            return LKQLUnit.INSTANCE;
        }

        /**
         * Execute the given checker array to the given Ada node.
         *
         * @param frame The frame to execute in.
         * @param currentStep The current step of the visiting.
         * @param currentNode The node to execute the checkers on.
         * @param checkers The checekrs to execute.
         * @param context The LKQL context.
         */
        private void executeCheckers(
                VirtualFrame frame,
                VisitStep currentStep,
                Libadalang.AdaNode currentNode,
                NodeChecker[] checkers,
                LKQLContext context) {
            // For each checker apply it on the current node if needed
            for (NodeChecker checker : checkers) {
                if (!currentStep.inGenericInstantiation()
                        || checker.isFollowGenericInstantiations()) {
                    try {
                        this.applyNodeRule(frame, checker, currentNode, context);
                    } catch (LangkitException e) {
                        // Report LAL exception only in debug mode
                        if (context.isCheckerDebug()) {
                            context.getDiagnosticEmitter()
                                    .emitDiagnostic(
                                            CheckerUtils.MessageKind.ERROR,
                                            e.getMsg(),
                                            new LalLocationWrapper(currentNode, context.linesCache),
                                            new SourceSectionWrapper(e.getLoc()),
                                            checker.getName());
                        }
                    } catch (LKQLRuntimeException e) {
                        context.getDiagnosticEmitter()
                                .emitDiagnostic(
                                        CheckerUtils.MessageKind.ERROR,
                                        e.getErrorMessage(),
                                        new LalLocationWrapper(currentNode, context.linesCache),
                                        e.getSourceLoc(),
                                        checker.getName());
                    }
                }
            }
        }

        /**
         * Apply the checker on the given node.
         *
         * @param frame The frame to execute the default arg value.
         * @param checker The checker to apply.
         * @param node The node to apply the checker on.
         * @param context The LKQL context.
         */
        private void applyNodeRule(
                VirtualFrame frame,
                NodeChecker checker,
                Libadalang.AdaNode node,
                LKQLContext context) {
            // Get the function for the checker
            LKQLFunction functionValue = checker.getFunction();
            String aliasName = checker.getAlias();
            String lowerRuleName = StringUtils.toLowerCase(checker.getName());

            // Prepare the arguments
            Object[] arguments = new Object[functionValue.parameterNames.length + 1];
            arguments[1] = node;
            for (int i = 1; i < functionValue.parameterDefaultValues.length; i++) {
                String paramName = functionValue.parameterNames[i];
                Object userDefinedArg =
                        context.getRuleArg(
                                (aliasName == null
                                        ? lowerRuleName
                                        : StringUtils.toLowerCase(aliasName)),
                                StringUtils.toLowerCase(paramName));
                arguments[i + 1] =
                        userDefinedArg == null
                                ? functionValue.parameterDefaultValues[i].executeGeneric(frame)
                                : userDefinedArg;
            }

            // Place the closure in the arguments
            arguments[0] = functionValue.closure.getContent();

            // Call the checker
            final boolean ruleResult;
            try {
                ruleResult =
                        LKQLTypeSystemGen.expectTruthy(
                                        interopLibrary.execute(functionValue, arguments))
                                .isTruthy();
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_BOOLEAN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        functionValue.getBody());
            } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
                // TODO: Move function runtime verification to the LKQLFunction class (#138)
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }

            if (ruleResult) {
                reportViolation(context, checker, node);
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
                LKQLContext context, NodeChecker checker, Libadalang.AdaNode node) {
            if (node instanceof Libadalang.BasicDecl basicDecl) {
                Libadalang.AdaNode definingName = basicDecl.pDefiningName();
                node = definingName.isNone() ? node : definingName;
            }
            context.getDiagnosticEmitter()
                    .emitRuleViolation(
                            checker,
                            checker.getMessage(),
                            new LalLocationWrapper(node, context.linesCache),
                            node.pGenericInstantiations(),
                            context);
        }

        // ----- Inner classes -----

        /**
         * This record contains the information for a visiting step.
         *
         * @param node The node to visit.
         * @param inGenericInstantiation Whether the visit is currently in a generic instantiation.
         */
        private record VisitStep(
                Libadalang.AdaNode node, boolean inGenericInstantiation, boolean inSparkCode) {}
    }
}
