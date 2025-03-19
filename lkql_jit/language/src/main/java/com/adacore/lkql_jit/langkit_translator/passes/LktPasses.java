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
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.ExprArg;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.nodes.declarations.*;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCallNodeGen;
import com.adacore.lkql_jit.nodes.expressions.FunExpr;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyDecl;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockExpr;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessWrapperNodeGen;
import com.adacore.lkql_jit.nodes.expressions.literals.BigIntegerLiteral;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.nodes.expressions.literals.LongLiteral;
import com.adacore.lkql_jit.nodes.expressions.literals.StringLiteral;
import com.adacore.lkql_jit.nodes.expressions.operators.*;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.nodes.patterns.FilteredPattern;
import com.adacore.lkql_jit.nodes.patterns.NullPattern;
import com.adacore.lkql_jit.nodes.patterns.OrPattern;
import com.adacore.lkql_jit.nodes.patterns.ParenPattern;
import com.adacore.lkql_jit.nodes.patterns.UniversalPattern;
import com.adacore.lkql_jit.nodes.patterns.*;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodeKindPattern;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
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

        public enum FrameKind {
            Concrete,
            Virtual,
            None,
        }

        /** Whether "node" needs a frame to be introduced to contain inner bindings. */
        private static FrameKind needsFrame(LktNode node) {
            if ((node instanceof FunDecl) || (node instanceof Liblktlang.LangkitRoot)) {
                return FrameKind.Concrete;
            } else if ((node instanceof Liblktlang.BlockExpr) || (node instanceof Liblktlang.Isa)) {
                return FrameKind.Virtual;
            } else {
                return FrameKind.None;
            }
        }

        /**
         * Return the binding name for "node", if it needs to be bound in the current scope, or null
         * if it should not be bound.
         */
        private static String getBindingName(LktNode node) {
            if (node instanceof Decl d && !(node instanceof FunArgDecl)) {
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

            if (node instanceof FunArgDecl funArgDecl) {
                builder.addParameter(funArgDecl.fSynName().getText());
            }

            var frameKind = needsFrame(node);
            switch (frameKind) {
                case Concrete -> builder.openFrame(node);
                case Virtual -> builder.openVirtualFrame(node);
                case None -> {}
            }

            // Visit children
            for (var c : node.children()) {
                if (!c.isNone()) {
                    recurseBuildFrames(c, builder);
                }
            }

            if (frameKind != FrameKind.None) {
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

        private String parseStringLiteral(final StringLit stringLit) {
            // Prepare the result
            final String res;

            // If the string literal is as simple one
            if (stringLit instanceof SingleLineStringLit) {
                String raw = stringLit.getText();
                res = StringUtils.translateEscapes(raw.substring(1, raw.length() - 1));
            }
            // If the string literal is a block iterate over all of its parts
            else {
                final StringBuilder builder = new StringBuilder();
                for (var line : ((BlockStringLit) stringLit).fLines().children()) {
                    var str = StringUtils.translateEscapes(line.getText().substring(2));

                    if (str.length() > 0) {
                        // First character should be a whitespace, as specified in
                        // the user manual.
                        if (str.charAt(0) != ' ') {
                            throw translationError(
                                stringLit,
                                "Invalid blockstring: first character should be whitespace"
                            );
                        }
                        builder.append(str.substring(1)).append("\n");
                    }
                }
                res = builder.toString().trim();
            }

            // Return the result
            return res;
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
                parameters.add(buildParam((FunArgDecl) param));
            }
            final var lkqlBody = buildExpr(body);

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

        public ParameterDeclaration buildParam(Liblktlang.FunArgDecl argDecl) {
            String name = argDecl.fSynName().getText();
            var defaultExpr = argDecl.fDefaultVal();
            return new ParameterDeclaration(
                loc(argDecl),
                name,
                defaultExpr.isNone() ? null : buildExpr(defaultExpr)
            );
        }

        private TopLevelList buildRoot(Liblktlang.LangkitRoot root) {
            frames.enterFrame(root);
            final List<LKQLNode> topLevelNodes = new ArrayList<>();

            for (var child : root.fDecls().children()) {
                topLevelNodes.add(buildDecl(((FullDecl) child).fDecl()));
            }

            frames.exitFrame();

            return new TopLevelList(
                loc(root),
                frames.getFrameDescriptor(),
                topLevelNodes.toArray(new LKQLNode[0]),
                source.isInteractive(),
                "" // TODO: Add module level doc support
            );
        }

        private Arg buildParam(Liblktlang.Param param) {
            if (param.fName().isNone()) {
                return new ExprArg(loc(param), buildExpr(param.fValue()));
            } else {
                return new NamedArg(
                    loc(param),
                    new Identifier(loc(param.fName()), param.fName().getText()),
                    buildExpr(param.fValue())
                );
            }
        }

        private Annotation buildAnnotation(Liblktlang.DeclAnnotation annotation) {
            return new Annotation(
                loc(annotation),
                annotation.fName().getText(),
                buildArgs(
                    Arrays.stream(annotation.fParams().fParams().children())
                        .map(p -> buildParam((Param) p))
                        .toList(),
                    loc(annotation.fParams())
                )
            );
        }

        private Declaration buildDecl(Liblktlang.Decl decl) {
            if (decl instanceof FunDecl funDecl) {
                final String name = funDecl.fSynName().getText();
                frames.declareBinding(name);
                final int slot = frames.getBinding(name);

                // Full decl
                var fullDecl = (FullDecl) funDecl.parent();

                // Annotation
                Annotation annotation = null;
                var annotations = fullDecl.fDeclAnnotations();
                if (annotations.getChildrenCount() > 0) {
                    annotation = buildAnnotation(
                        (Liblktlang.DeclAnnotation) annotations.getChild(0)
                    );
                }

                var ret = new FunctionDeclaration(
                    loc(funDecl),
                    annotation,
                    name,
                    slot,
                    createFunExpr(funDecl, fullDecl.fDoc(), funDecl.fBody(), funDecl.fArgs())
                );

                if (annotation != null) {
                    if (annotation.getName().equals(Constants.ANNOTATION_NODE_CHECK)) {
                        return new CheckerExport(
                            loc(funDecl),
                            annotation,
                            CheckerExport.CheckerMode.NODE,
                            ret
                        );
                    } else if (annotation.getName().equals(Constants.ANNOTATION_UNIT_CHECK)) {
                        return new CheckerExport(
                            loc(funDecl),
                            annotation,
                            CheckerExport.CheckerMode.UNIT,
                            ret
                        );
                    }
                }
                return ret;
            } else if (decl instanceof ValDecl valDecl) {
                var name = valDecl.fSynName().getText();
                var value = buildExpr(valDecl.fExpr());
                frames.declareBinding(name);
                final int slot = frames.getBinding(name);
                return new ValueDeclaration(loc(valDecl), slot, value);
            } else {
                throw LKQLRuntimeException.fromMessage(
                    "Translation for " + decl.getKind() + " not implemented"
                );
            }
        }

        private Expr buildExpr(Liblktlang.Expr expr) {
            if (expr instanceof NumLit numLit) {
                try {
                    return new LongLiteral(loc(numLit), Long.parseLong(numLit.getText()));
                } catch (NumberFormatException e) {
                    return new BigIntegerLiteral(loc(numLit), new BigInteger(numLit.getText()));
                }
            } else if (expr instanceof CallExpr callExpr) {
                final Expr callee = buildExpr(callExpr.fName());
                final ArgList arguments = buildArgs(
                    Arrays.stream(callExpr.fArgs().children())
                        .map(a -> buildParam((Param) a))
                        .toList(),
                    loc(callExpr.fArgs())
                );
                return FunCallNodeGen.create(loc(callExpr), false, arguments, callee);
            } else if (expr instanceof Liblktlang.BlockExpr blockExpr) {
                frames.enterFrame(blockExpr);
                final List<BlockBody> blockBody = new ArrayList<>();
                for (var valDecl : blockExpr.fValDefs().children()) {
                    blockBody.add(new BlockBodyDecl(loc(valDecl), buildDecl((ValDecl) valDecl)));
                }
                var subExpr = buildExpr(blockExpr.fExpr());
                frames.exitFrame();
                return new BlockExpr(loc(blockExpr), blockBody.toArray(new BlockBody[0]), subExpr);
            } else if (expr instanceof RefId id) {
                // In LKQL, true & false are keywords. In Lkt, they're just an instance of the Bool
                // enum. Waiting for more generalized handling of Lkt enums in LKQL, we'll just do
                // a manual translation here
                if (Objects.equals(id.getText(), "true")) {
                    return new BooleanLiteral(loc(id), true);
                } else if (Objects.equals(id.getText(), "false")) {
                    return new BooleanLiteral(loc(id), false);
                }

                return buildRead(id.getText(), loc(id));
            } else if (expr instanceof Isa isA) {
                // Translate the is clause node expression
                var nodeExpr = buildExpr(isA.fExpr());
                frames.enterFrame(isA);
                var pattern = buildPattern(isA.fPattern());
                frames.exitFrame();
                return IsClauseNodeGen.create(loc(isA), pattern, nodeExpr);
            } else if (expr instanceof StringLit stringLit) {
                return new StringLiteral(loc(stringLit), parseStringLiteral(stringLit));
            } else if (expr instanceof Liblktlang.BinOp binOp) {
                final var left = buildExpr(binOp.fLeft());
                final var right = buildExpr(binOp.fRight());

                final SourceSection location = loc(binOp);

                // Create the binary operator by switching on the operator type
                return switch (binOp.fOp().getKind()) {
                    case OP_PLUS -> BinPlusNodeGen.create(location, left, right);
                    case OP_MINUS -> BinMinusNodeGen.create(location, left, right);
                    case OP_MULT -> BinMulNodeGen.create(location, left, right);
                    case OP_DIV -> BinDivNodeGen.create(location, left, right);
                    case OP_AND -> new BinAnd(location, left, right);
                    case OP_OR -> new BinOr(location, left, right);
                    case OP_EQ -> BinEqNodeGen.create(location, left, right);
                    case OP_NE -> BinNeqNodeGen.create(location, left, right);
                    case OP_AMP -> BinConcatNodeGen.create(location, left, right);
                    case OP_LT -> BinLtNodeGen.create(location, left, right);
                    case OP_LTE -> BinLeqNodeGen.create(location, left, right);
                    case OP_GT -> BinGtNodeGen.create(location, left, right);
                    case OP_GTE -> BinGeqNodeGen.create(location, left, right);
                    default -> null;
                };
            } else if (expr instanceof NotExpr notExpr) {
                return UnNotNodeGen.create(loc(notExpr), buildExpr(notExpr.fExpr()));
            } else if (expr instanceof DotExpr dotExpr) {
                final var memberIdentifier = dotExpr.fSuffix();
                return DotAccessWrapperNodeGen.create(
                    loc(dotExpr),
                    DotAccessNodeGen.create(
                        loc(dotExpr),
                        new Identifier(loc(memberIdentifier), memberIdentifier.getText()),
                        buildExpr(dotExpr.fPrefix())
                    )
                );
            } else {
                throw LKQLRuntimeException.fromMessage(
                    "Translation for " + expr.getKind() + " not implemented"
                );
            }
        }

        private BasePattern buildPattern(Liblktlang.BasePattern pattern) {
            if (pattern instanceof Liblktlang.NullPattern nullPattern) {
                return new NullPattern(loc(nullPattern));
            } else if (pattern instanceof Liblktlang.UniversalPattern univPattern) {
                return new UniversalPattern(loc(univPattern));
            } else if (pattern instanceof Liblktlang.RegexPattern regexPattern) {
                var regex = regexPattern.getText();
                regex = regex.substring(1, regex.length() - 1);
                return RegexPatternNodeGen.create(loc(regexPattern), regex);
            } else if (pattern instanceof Liblktlang.TuplePattern tuplePattern) {
                // Get the sub-patterns inside the tuple pattern
                final List<BasePattern> tuplePatterns = new ArrayList<>();
                for (var p : tuplePattern.fPatterns().children()) {
                    tuplePatterns.add(buildPattern((Liblktlang.BasePattern) p));
                }
                return TuplePatternNodeGen.create(
                    loc(tuplePattern),
                    tuplePatterns.toArray(new BasePattern[0])
                );
            } else if (pattern instanceof Liblktlang.ParenPattern parenPattern) {
                final BasePattern p = buildPattern(parenPattern.fPattern());
                return new ParenPattern(loc(parenPattern), p);
            } else if (pattern instanceof Liblktlang.OrPattern orPattern) {
                return new OrPattern(
                    loc(orPattern),
                    buildPattern(orPattern.fLeft()),
                    buildPattern(orPattern.fRight())
                );
            } else if (pattern instanceof Liblktlang.FilteredPattern filteredPattern) {
                return new FilteredPattern(
                    loc(filteredPattern),
                    buildPattern(filteredPattern.fPattern()),
                    buildExpr(filteredPattern.fPredicate())
                );
            } else if (pattern instanceof Liblktlang.TypePattern typePattern) {
                return new NodeKindPattern(loc(typePattern), typePattern.fTypeName().getText());
            } else {
                throw LKQLRuntimeException.fromMessage(
                    "Translation for " + pattern.getKind() + " not implemented"
                );
            }
        }
    }

    public static LKQLNode buildLKQLNode(Source source, LangkitRoot root, ScriptFrames frames) {
        return new TranslationPass(source, frames).buildRoot(root);
    }
}
