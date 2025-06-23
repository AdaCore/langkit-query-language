//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.checker.built_ins;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.annotations.BuiltInFunction;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.checker.NodeChecker;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.functions.FileUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.LangkitLocationWrapper;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;

public final class NodeCheckerFunction {

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
            // Get the arguments
            final LKQLContext context = LKQLLanguage.getContext(this);
            final LangkitSupport.AnalysisUnit rootUnit;

            final NodeChecker[] allNodeCheckers = context.getAllNodeCheckers();
            final NodeChecker[] nodeCheckers = context.getNodeCheckers();
            final NodeChecker[] sparkNodeCheckers = context.getSparkNodeCheckers();
            final boolean mustFollowInstantiations = context.mustFollowInstantiations();
            final boolean hasSparkCheckers = sparkNodeCheckers.length > 0;

            try {
                root = LKQLTypeSystemGen.expectNodeInterface(frame.getArguments()[0]);
                rootUnit = root.getUnit();
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.NODE_INTERFACE,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this
                );
            }

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
                    var callerLocation = LKQLRuntimeException.getClosestNodeWithSourceInfo(
                        stackFrame.getLocation()
                    );

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
                        // No need to check if the child is a base subprogram body in SPARK mode if
                        // there is no
                        // required
                        // SPARK checkers. This avoids useless calls to 'pIsSubjectToProof'.
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

            // If the engine is in fixing mode and the current unit has some modification
            if (
                context.getEngineMode() == LKQLOptions.EngineMode.FIXER &&
                context.hasRewritingContext()
            ) {
                // Apply the current rewriting context
                final var applyRes = context.applyOrCloseRewritingContext();
                if (!applyRes.success) {
                    final var message = StringUtils.concat(
                        "Error(s) while applying a rewriting context: ",
                        ArrayUtils.toString(applyRes.getDiagnostics())
                    );
                    throw LKQLRuntimeException.fromMessage(message);
                }

                // Then output the rewrote unit as required
                final var patchedSource = rootUnit.getText();
                final var basePatchedFileName = rootUnit.getFileName(false);
                final var fullPatchedFileName = rootUnit.getFileName(true);
                switch (context.getAutoFixMode()) {
                    case DISPLAY:
                        // If the required action is the display, just print the patched unit
                        var header = StringUtils.concat(
                            "Patched \"",
                            rootUnit.getFileName(false),
                            "\":\n"
                        );
                        header = StringUtils.concat(header, "=".repeat(header.length() - 1), "\n");
                        context.println(header);
                        context.println(patchedSource);
                        break;
                    case NEW_FILE:
                        // If the required action is create a new file, create a new file alongside
                        // the original file and dump the patched unit inside it.
                        // The new file name is formed as this: <original_file_name>.patched.
                        final var newFile = FileUtils.create(
                            StringUtils.concat(fullPatchedFileName, ".patched")
                        );
                        writeInFile(newFile, patchedSource);
                        context.println(
                            StringUtils.concat(
                                "File \"",
                                basePatchedFileName,
                                "\" has been patched (result in \"",
                                basePatchedFileName,
                                ".patched\")"
                            )
                        );
                        break;
                    case PATCH_FILE:
                        // If the required action is to patch the existing file, just replace the
                        // content of the original file with the patched unit.
                        final var originalFile = FileUtils.create(fullPatchedFileName);
                        writeInFile(originalFile, patchedSource);
                        context.println(
                            StringUtils.concat(
                                "File \"",
                                basePatchedFileName,
                                "\" has been patched"
                            )
                        );
                        break;
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
            LangkitSupport.NodeInterface currentNode,
            NodeChecker[] checkers,
            LKQLContext context
        ) {
            // For each checker apply it on the current node if needed
            for (NodeChecker checker : checkers) {
                if (
                    !currentStep.inGenericInstantiation() || checker.isFollowGenericInstantiations()
                ) {
                    try {
                        this.applyNodeRule(frame, checker, currentNode, context);
                    } catch (LangkitException e) {
                        // Report LAL exception only in debug mode
                        if (context.isCheckerDebug()) {
                            context
                                .getDiagnosticEmitter()
                                .emitDiagnostic(
                                    CheckerUtils.MessageKind.ERROR,
                                    e.getMsg(),
                                    new LangkitLocationWrapper(currentNode, context.linesCache),
                                    new SourceSectionWrapper(e.getLoc()),
                                    checker.getName()
                                );
                        }
                    } catch (LKQLRuntimeException e) {
                        context
                            .getDiagnosticEmitter()
                            .emitDiagnostic(
                                CheckerUtils.MessageKind.ERROR,
                                e.getErrorMessage(),
                                new LangkitLocationWrapper(currentNode, context.linesCache),
                                e.getSourceLoc(),
                                checker.getName()
                            );
                    }
                }
            }
        }

        /**
         * Apply the checker on the given node.
         */
        private void applyNodeRule(
            VirtualFrame frame,
            NodeChecker checker,
            LangkitSupport.NodeInterface node,
            LKQLContext context
        ) {
            // Get the function for the checker
            LKQLFunction functionValue = checker.getFunction();
            String aliasName = checker.getAlias();
            String lowerRuleName = StringUtils.toLowerCase(checker.getName());

            // Prepare the arguments
            Object[] arguments = new Object[functionValue.parameterNames.length + 1];
            arguments[1] = node;
            for (int i = 1; i < functionValue.getParameterDefaultValues().length; i++) {
                String paramName = functionValue.parameterNames[i];
                Object userDefinedArg = context.getRuleArg(
                    (aliasName == null ? lowerRuleName : StringUtils.toLowerCase(aliasName)),
                    StringUtils.toLowerCase(paramName)
                );
                arguments[i + 1] = userDefinedArg == null
                    ? functionValue.getParameterDefaultValues()[i].executeGeneric(frame)
                    : userDefinedArg;
            }

            // Place the closure in the arguments
            arguments[0] = functionValue.closure.getContent();

            // Call the checker
            final boolean ruleResult;
            try {
                ruleResult = LKQLTypeSystemGen.expectTruthy(
                    interopLibrary.execute(functionValue, arguments)
                ).isTruthy();
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    functionValue.getBody()
                );
            } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
                // TODO: Move function runtime verification to the LKQLFunction class (#138)
                throw LKQLRuntimeException.fromJavaException(e, this);
            }

            if (ruleResult) {
                if (context.getEngineMode() == LKQLOptions.EngineMode.CHECKER) {
                    reportViolation(context, checker, node);
                } else if (
                    context.getEngineMode() == LKQLOptions.EngineMode.FIXER &&
                    checker.autoFix != null
                ) {
                    callAutoFix(
                        context,
                        context.getRewritingContext(),
                        interopLibrary,
                        checker,
                        node
                    );
                }
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

        /**
         * If the provided checker's auto fix is requested by the user, call it with the given node
         * and the current rewriting context.
         */
        @CompilerDirectives.TruffleBoundary
        private static void callAutoFix(
            LKQLContext context,
            LangkitSupport.RewritingContextInterface rewritingContext,
            InteropLibrary interopLibrary,
            NodeChecker checker,
            LangkitSupport.NodeInterface node
        ) {
            try {
                interopLibrary.execute(
                    checker.autoFix,
                    checker.autoFix.closure.getContent(),
                    node,
                    rewritingContext
                );
            } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
                // TODO: Move function runtime verification to the LKQLFunction class (#138)
                throw LKQLRuntimeException.fromJavaException(e, checker.autoFix.getBody());
            }
        }

        /** Util internal function to write in a file out of the Truffle bounds. */
        @CompilerDirectives.TruffleBoundary
        private void writeInFile(File file, String content) {
            try (final var writer = new FileWriter(file)) {
                file.createNewFile();
                writer.write(content);
            } catch (IOException e) {
                throw LKQLRuntimeException.fromJavaException(e, this);
            }
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
