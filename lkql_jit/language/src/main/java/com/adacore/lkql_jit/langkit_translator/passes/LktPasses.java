//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import static com.adacore.liblktlang.Liblktlang.*;

import com.adacore.liblktlang.Liblktlang;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFrames;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFramesBuilder;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.declarations.Annotation;
import com.adacore.lkql_jit.nodes.declarations.Declaration;
import com.adacore.lkql_jit.nodes.declarations.FunctionDeclaration;
import com.adacore.lkql_jit.nodes.declarations.ParameterDeclaration;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunExpr;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyDecl;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockExpr;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;
import java.util.List;

public final class LktPasses {

    private static boolean needsFrame(LktNode node) {
        return (node instanceof FunDecl);
    }

    private static String getBindingName(LktNode node) {
        if (node instanceof FunDecl fd) {
            return fd.fSynName().getText();
        } else if (node instanceof ValDecl vd) {
            return vd.fSynName().getText();
        }
        return null;
    }

    private static void recurseBuildFrames(LktNode node, ScriptFramesBuilder builder) {

        var bindingName = getBindingName(node);
        if (bindingName != null) {
            builder.addBinding(bindingName);
        }
        if (needsFrame(node)) {
            builder.openFrame(node);
        }

        if (node instanceof FunDecl funDecl) {
            final String symbol = funDecl.fSynName().getText();
            builder.addBinding(symbol);
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

    public static ScriptFramesBuilder buildFrames(LktNode root) {
        final ScriptFramesBuilder builder = new ScriptFramesBuilder();
        recurseBuildFrames(root, builder);
        return builder;
    }

    private static class TranslationPass {

        final Source source;
        final ScriptFrames frames;

        private SourceSection loc(final LktNode node) {
            return SourceSectionWrapper.createSection(node.getSourceLocationRange(), this.source);
        }

        private TranslationPass(Source source, ScriptFrames frames) {
            this.source = source;
            this.frames = frames;
        }

        private FunExpr funExprHelper(
                LktNode function, StringLit doc, Liblktlang.Expr body, LktNodeBaseList params) {
            this.frames.enterFrame(function);

            final List<ParameterDeclaration> parameters = new ArrayList<>();
            for (LktNode param : params.children()) {
                parameters.add((ParameterDeclaration) buildNode(param));
            }
            final var lkqlBody = (Expr) buildNode(body);

            final FunExpr res =
                    new FunExpr(
                            loc(body),
                            this.frames.getFrameDescriptor(),
                            this.frames.getClosureDescriptor(),
                            parameters.toArray(new ParameterDeclaration[0]),
                            doc.isNone() ? "" : doc.pDenotedValue().value,
                            lkqlBody);

            this.frames.exitFrame();

            return res;
        }

        private LKQLNode buildNode(LktNode node) {

            if (node == null) {
                return null;
            }

            switch (node) {
                case FunDecl funDecl -> {
                    final String name = getBindingName(funDecl);
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

                    var funExpr =
                            funExprHelper(
                                    funDecl, fullDecl.fDoc(), funDecl.fBody(), funDecl.fArgs());

                    return new FunctionDeclaration(loc(funDecl), annotation, name, slot, funExpr);
                }

                case Liblktlang.BlockExpr blockExpr -> {
                    frames.enterFrame(blockExpr);

                    // Create the declaration list
                    final List<BlockBody> blockBody = new ArrayList<>();
                    for (var valDecl : blockExpr.fValDefs().children()) {
                        blockBody.add(
                                new BlockBodyDecl(loc(valDecl), (Declaration) buildNode(valDecl)));
                    }

                    // Create the expr
                    var expr = (Expr) buildNode(blockExpr.fExpr());

                    frames.exitFrame();

                    return new BlockExpr(loc(blockExpr), blockBody.toArray(new BlockBody[0]), expr);
                }

                default -> {
                    return null;
                }
            }
        }
    }

    public static LKQLNode buildLKQLNode(Source source, LktNode root, ScriptFrames frames) {
        return new TranslationPass(source, frames).buildNode(root);
    }
}
