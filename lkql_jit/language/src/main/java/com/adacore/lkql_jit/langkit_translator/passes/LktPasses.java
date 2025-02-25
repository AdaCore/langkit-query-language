//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import static com.adacore.liblktlang.Liblktlang.*;

import com.adacore.liblktlang.Liblktlang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFrames;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFramesBuilder;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.ExprArg;
import com.adacore.lkql_jit.nodes.declarations.*;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCallNodeGen;
import com.adacore.lkql_jit.nodes.expressions.FunExpr;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyDecl;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockExpr;
import com.adacore.lkql_jit.nodes.expressions.literals.BigIntegerLiteral;
import com.adacore.lkql_jit.nodes.expressions.literals.LongLiteral;
import com.oracle.truffle.api.source.Source;
import java.math.BigInteger;
import java.util.*;

/** Namespace class containing all passes to expand from Lkt syntax to the LKQL Truffle tree. */
public final class LktPasses {

    /**
     * Container class containing utilities related to the framing pass. The framing pass itself is
     * a simple function (buildFrames).
     *
     * <p>The function itself should not need to change. Instead, needsFrame and getBindingName can
     * be extended to accommodate for new node types.
     */
    public static final class Frames {

        /** Whether "node" needs a frame to be introduced to contain inner bindings. */
        private static boolean needsFrame(LktNode node) {
            return (
                (node instanceof FunDecl) ||
                (node instanceof Liblktlang.BlockExpr) ||
                (node instanceof Liblktlang.LangkitRoot)
            );
        }

        /**
         * Return the binding name for "node", if it needs to be bound in the current scope, or null
         * if it should not be bound.
         */
        private static String getBindingName(LktNode node) {
            if (node instanceof Decl d) {
                return d.fSynName().getText();
            }
            return null;
        }

        /**
         * Internal helper for the public "buildFrames" pass, doing all the work of recursing on the
         * nodes and building the frame tree.
         */
        private static void recurseBuildFrames(LktNode node, ScriptFramesBuilder builder) {
            var bindingName = getBindingName(node);
            if (bindingName != null) {
                builder.addBinding(bindingName);
            }

            if (needsFrame(node)) {
                builder.openFrame(node);
            }

            // Visit children
            for (var c : node.children()) {
                if (!c.isNone()) {
                    recurseBuildFrames(c, builder);
                }
            }

            if (needsFrame(node)) {
                builder.closeFrame();
            }
        }

        /** Public pass to build a frame tree from an LktNode root node. */
        public static ScriptFramesBuilder buildFrames(LktNode root) {
            final ScriptFramesBuilder builder = new ScriptFramesBuilder();
            recurseBuildFrames(root, builder);
            return builder;
        }
    }

    private static class TranslationPass extends BaseTranslationPass {

        public TranslationPass(Source source, ScriptFrames frames) {
            super(source, frames);
        }

        /**
         * Helper to create a function expression. The signature is complex because there is no
         * abstraction for a function expression in Lkt.
         *
         * <p>This has been pre-factorized but is only used once. The goal is to use it once we
         * expand lambda expressions.
         */
        private FunExpr createFunExpr(
            LktNode function,
            StringLit doc,
            Liblktlang.Expr body,
            LktNodeBaseList params
        ) {
            this.frames.enterFrame(function);

            final List<ParameterDeclaration> parameters = new ArrayList<>();
            for (LktNode param : params.children()) {
                parameters.add((ParameterDeclaration) buildNode(param));
            }
            final var lkqlBody = (Expr) buildNode(body);

            final FunExpr res = new FunExpr(
                loc(body),
                this.frames.getFrameDescriptor(),
                this.frames.getClosureDescriptor(),
                parameters.toArray(new ParameterDeclaration[0]),
                doc.isNone() ? "" : doc.pDenotedValue().value,
                lkqlBody
            );

            this.frames.exitFrame();

            return res;
        }

        /**
         * Private recursive function that does most of the heavy lifting for this pass.
         *
         * <p>One question is whether we could cut this function into several sub-functions (at
         * least buildDecl/buildExpr/etc) to avoid some casting/dynamic typing.
         */
        private LKQLNode buildNode(LktNode node) {
            // General null guard allows us to not handle null case in every recursive call to
            // buildNode
            if (node == null) {
                return null;
            }

            if (node instanceof FunDecl funDecl) {
                final String name = funDecl.fSynName().getText();
                frames.declareBinding(name);
                final int slot = frames.getBinding(name);

                // Full decl
                var fullDecl = (FullDecl) funDecl.parent();

                // Annotation
                Annotation annotation = null;
                var annotations = fullDecl.fDeclAnnotations();
                if (annotations.getChildrenCount() > 0) {
                    annotation = (Annotation) buildNode(annotations.getChild(0));
                }

                var funExpr = createFunExpr(
                    funDecl,
                    fullDecl.fDoc(),
                    funDecl.fBody(),
                    funDecl.fArgs()
                );

                return new FunctionDeclaration(loc(funDecl), annotation, name, slot, funExpr);
            } else if (node instanceof Liblktlang.BlockExpr blockExpr) {
                frames.enterFrame(blockExpr);

                // Create the declaration list
                final List<BlockBody> blockBody = new ArrayList<>();
                for (var valDecl : blockExpr.fValDefs().children()) {
                    blockBody.add(
                        new BlockBodyDecl(loc(valDecl), (Declaration) buildNode(valDecl))
                    );
                }

                // Create the expr
                var expr = (Expr) buildNode(blockExpr.fExpr());

                frames.exitFrame();

                return new BlockExpr(loc(blockExpr), blockBody.toArray(new BlockBody[0]), expr);
            } else if (node instanceof CallExpr callExpr) {
                final Expr callee = (Expr) buildNode(callExpr.fName());
                final ArgList arguments = (ArgList) buildNode(callExpr.fArgs());

                return FunCallNodeGen.create(loc(callExpr), false, arguments, callee);
            } else if (node instanceof DeclAnnotation declAnnotation) {
                return new Annotation(
                    loc(declAnnotation),
                    declAnnotation.fName().getText(),
                    (ArgList) buildNode(declAnnotation.fParams())
                );
            } else if (node instanceof LangkitRoot lktRoot) {
                frames.enterFrame(lktRoot);
                final List<LKQLNode> topLevelNodes = new ArrayList<>();

                for (var child : lktRoot.fDecls().children()) {
                    topLevelNodes.add(buildNode(child));
                }

                frames.exitFrame();

                return new TopLevelList(
                    loc(lktRoot),
                    frames.getFrameDescriptor(),
                    topLevelNodes.toArray(new LKQLNode[0]),
                    source.isInteractive(),
                    "" // TODO: Add module level doc support
                );
            } else if (node instanceof FullDecl fullDecl) {
                // Nothing special to do here, because specific annotations are handled in
                // sub-cases in children
                return buildNode(fullDecl.fDecl());
            } else if (node instanceof NumLit numLit) {
                try {
                    return new LongLiteral(loc(numLit), Long.parseLong(numLit.getText()));
                } catch (NumberFormatException e) {
                    return new BigIntegerLiteral(loc(numLit), new BigInteger(numLit.getText()));
                }
            } else if (node instanceof ValDecl valDecl) { // Translate the declaration fields
                final String name = valDecl.fSynName().getText();
                final Expr value = (Expr) buildNode(valDecl.fExpr());

                // Get the slot for the name
                frames.declareBinding(name);
                final int slot = frames.getBinding(name);

                // Return the value declaration node
                return new ValueDeclaration(loc(valDecl), slot, value);
            } else if (node instanceof RefId id) {
                return buildRead(id.getText(), loc(id));
            } else if (node instanceof ParamList paramList) {
                return buildArgs(
                    Arrays.stream(paramList.children()).map(a -> (Arg) buildNode(a)).toList(),
                    loc(paramList)
                );
            } else if (node instanceof Param param) {
                if (param.fName().isNone()) {
                    return new ExprArg(loc(param), (Expr) buildNode(param.fValue()));
                }
            } else {
                throw LKQLRuntimeException.fromMessage(
                    "Translation for " + node.getKind() + " not implemented"
                );
            }

            return null;
        }
    }

    public static LKQLNode buildLKQLNode(Source source, LktNode root, ScriptFrames frames) {
        return new TranslationPass(source, frames).buildNode(root);
    }
}
