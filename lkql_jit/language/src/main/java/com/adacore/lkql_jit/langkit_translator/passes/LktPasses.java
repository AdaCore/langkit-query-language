//
//  Copyright (C) 2005-2025, AdaCore
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
import com.adacore.lkql_jit.nodes.expressions.literals.*;
import com.adacore.lkql_jit.nodes.expressions.match.Match;
import com.adacore.lkql_jit.nodes.expressions.match.MatchArm;
import com.adacore.lkql_jit.nodes.expressions.operators.*;
import com.adacore.lkql_jit.nodes.patterns.BindingPattern;
import com.adacore.lkql_jit.nodes.patterns.FilteredPattern;
import com.adacore.lkql_jit.nodes.patterns.NullPattern;
import com.adacore.lkql_jit.nodes.patterns.OrPattern;
import com.adacore.lkql_jit.nodes.patterns.ParenPattern;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.adacore.lkql_jit.nodes.patterns.UniversalPattern;
import com.adacore.lkql_jit.nodes.patterns.*;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.ExtendedNodePattern;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodeKindPattern;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodePatternDetail;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodePatternFieldNodeGen;
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
            } else if (
                node instanceof Liblktlang.BlockExpr ||
                node instanceof Liblktlang.Isa ||
                node instanceof Liblktlang.PatternMatchBranch
            ) {
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
            if (node instanceof Decl d && !(node instanceof FunParamDecl)) {
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

            if (node instanceof FunParamDecl funParamDecl) {
                builder.addParameter(funParamDecl.fSynName().getText());
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
                res = StringUtils.translateEscapes(
                    stringLit.getText().substring(1, stringLit.getText().length() - 1)
                );
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
         */
        private FunExpr createFunExpr(
            LktNode function,
            String functionName,
            StringLit doc,
            Liblktlang.Expr functionBody,
            LktNodeBaseList params
        ) {
            this.frames.enterFrame(function);

            var paramNames = new String[params.getChildrenCount()];
            var defaultVals = new Expr[params.getChildrenCount()];

            for (int i = 0; i < params.getChildrenCount(); i++) {
                var paramDecl = (FunParamDecl) params.getChild(i);
                paramNames[i] = paramDecl.fSynName().getText();
                defaultVals[i] = paramDecl.fDefaultVal().isNone()
                    ? null
                    : buildExpr(paramDecl.fDefaultVal());
            }
            final var body = buildExpr(functionBody);

            final FunExpr res = new FunExpr(
                loc(functionBody),
                this.frames.getFrameDescriptor(),
                this.frames.getClosureDescriptor(),
                paramNames,
                defaultVals,
                doc.isNone() ? "" : doc.pDenotedValue().value,
                body,
                functionName
            );

            this.frames.exitFrame();

            return res;
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
                "" // TODO: Add module level doc support (see eng/libadalang/langkit-query-language#436)
            );
        }

        private Arg buildArg(Liblktlang.Argument arg) {
            if (arg.fName().isNone()) {
                return new ExprArg(loc(arg), buildExpr(arg.fValue()));
            } else {
                return new NamedArg(
                    loc(arg),
                    new Identifier(loc(arg.fName()), arg.fName().getText()),
                    buildExpr(arg.fValue())
                );
            }
        }

        private Annotation buildAnnotation(Liblktlang.DeclAnnotation annotation) {
            return new Annotation(
                loc(annotation),
                annotation.fName().getText(),
                buildArgs(
                    Arrays.stream(annotation.fArgs().fArgs().children())
                        .map(a -> buildArg((Argument) a))
                        .toList(),
                    loc(annotation.fArgs())
                )
            );
        }

        private Declaration buildDecl(Liblktlang.Decl decl) {
            if (decl instanceof Liblktlang.FunDecl funDecl) {
                final String name = funDecl.fSynName().getText();
                frames.declareBinding(name);
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
                    frames.getBinding(name),
                    createFunExpr(
                        funDecl,
                        name,
                        fullDecl.fDoc(),
                        funDecl.fBody(),
                        funDecl.fParams()
                    )
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
                return new ValueDeclaration(loc(valDecl), frames.getBinding(name), value);
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
            } else if (expr instanceof NullLit nullLit) {
                return new NullLiteral(loc(nullLit));
            } else if (expr instanceof CallExpr callExpr) {
                final Expr callee = buildExpr(callExpr.fName());
                final ArgList arguments = buildArgs(
                    Arrays.stream(callExpr.fArgs().children())
                        .map(a -> buildArg((Argument) a))
                        .toList(),
                    loc(callExpr.fArgs())
                );
                return FunCallNodeGen.create(
                    loc(callExpr),
                    false,
                    Arrays.stream(arguments.getArgs()).map(Arg::getArgExpr).toArray(Expr[]::new),
                    Arrays.stream(arguments.getArgs())
                        .map(Arg::getArgStringName)
                        .toArray(String[]::new),
                    callee
                );
            } else if (expr instanceof Liblktlang.BlockExpr blockExpr) {
                frames.enterFrame(blockExpr);
                final var blockBody = new BlockBody[blockExpr.fClauses().getChildrenCount() - 1];
                for (int i = 0; i < blockBody.length; i++) {
                    final var clause = (Liblktlang.BlockExprClause) blockExpr
                        .fClauses()
                        .getChild(i);
                    final var valDecl =
                        (Liblktlang.ValDecl) ((Liblktlang.FullDecl) clause.fClause()).fDecl();
                    blockBody[i] = new BlockBodyDecl(loc(valDecl), buildDecl(valDecl));
                }
                final var subExpr = buildExpr(
                    (Liblktlang.Expr) blockExpr
                        .fClauses()
                        .getChild(blockExpr.fClauses().getChildrenCount() - 1)
                );
                frames.exitFrame();
                return new BlockExpr(loc(blockExpr), blockBody, subExpr);
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
                    case OP_AND -> BinAndNodeGen.create(location, left, right);
                    case OP_OR -> BinOrNodeGen.create(location, left, right);
                    case OP_EQ -> BinEqNodeGen.create(location, left, right);
                    case OP_NE -> UnNotNodeGen.create(
                        location,
                        BinEqNodeGen.create(location, left, right)
                    );
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
            } else if (expr instanceof MatchExpr matchExpr) {
                Expr matchVal = buildExpr(matchExpr.fMatchExpr());
                var arms = Arrays.stream(matchExpr.fBranches().children())
                    .map(b -> (PatternMatchBranch) b)
                    .map(b -> {
                        frames.enterFrame(b);
                        var pattern = buildPattern(b.fPattern());
                        var branchExpr = buildExpr(b.fExpr());
                        frames.exitFrame();
                        return new MatchArm(loc(b), pattern, branchExpr);
                    })
                    .toList()
                    .toArray(new MatchArm[0]);
                return new Match(loc(matchExpr), matchVal, arms);
            } else {
                throw LKQLRuntimeException.fromMessage(
                    "Translation for " + expr.getKind() + " not implemented"
                );
            }
        }

        private Pattern buildPattern(Liblktlang.Pattern pattern) {
            if (pattern instanceof Liblktlang.NullPattern nullPattern) {
                return new NullPattern(loc(nullPattern));
            } else if (pattern instanceof Liblktlang.IntegerPattern integerPattern) {
                try {
                    return IntegerPatternNodeGen.create(
                        loc(integerPattern),
                        Integer.parseInt(integerPattern.getText())
                    );
                } catch (NumberFormatException e) {
                    throw translationError(integerPattern, "Invalid number literal for pattern");
                }
            } else if (pattern instanceof Liblktlang.AnyTypePattern univPattern) {
                return new UniversalPattern(loc(univPattern));
            } else if (pattern instanceof Liblktlang.RegexPattern regexPattern) {
                var regex = regexPattern.getText();
                regex = regex.substring(1, regex.length() - 1);
                return RegexPatternNodeGen.create(loc(regexPattern), regex);
            } else if (pattern instanceof Liblktlang.TuplePattern tuplePattern) {
                // Get the sub-patterns inside the tuple pattern
                final List<Pattern> tuplePatterns = new ArrayList<>();
                for (var p : tuplePattern.fSubPatterns().children()) {
                    tuplePatterns.add(buildPattern((Liblktlang.Pattern) p));
                }
                return TuplePatternNodeGen.create(
                    loc(tuplePattern),
                    tuplePatterns.toArray(new Pattern[0])
                );
            } else if (pattern instanceof Liblktlang.ParenPattern parenPattern) {
                final Pattern p = buildPattern(parenPattern.fSubPattern());
                return new ParenPattern(loc(parenPattern), p);
            } else if (pattern instanceof Liblktlang.OrPattern orPattern) {
                return new OrPattern(
                    loc(orPattern),
                    buildPattern(orPattern.fLeftSubPattern()),
                    buildPattern(orPattern.fRightSubPattern())
                );
            } else if (pattern instanceof Liblktlang.FilteredPattern filteredPattern) {
                return new FilteredPattern(
                    loc(filteredPattern),
                    buildPattern(filteredPattern.fSubPattern()),
                    buildExpr(filteredPattern.fPredicate())
                );
            } else if (pattern instanceof Liblktlang.TypePattern typePattern) {
                return new NodeKindPattern(loc(typePattern), typePattern.fTypeName().getText());
            } else if (pattern instanceof Liblktlang.ExtendedPattern extendedPattern) {
                // Translate the extended node pattern fields
                final Pattern nodePattern = buildPattern(extendedPattern.fSubPattern());

                // Get the pattern details
                final List<NodePatternDetail> details = new ArrayList<>();
                for (var patternDetail : extendedPattern.fDetails()) {
                    details.add(buildPatternDetail(patternDetail));
                }

                // Return the new extended node pattern node
                return new ExtendedNodePattern(
                    loc(extendedPattern),
                    nodePattern,
                    details.toArray(new NodePatternDetail[0])
                );
            } else if (pattern instanceof Liblktlang.BindingPattern bindingPattern) {
                // Get the binding value name
                final String name = bindingPattern.fDecl().fSynName().getText();

                // Get the slot of the binding
                this.frames.declareBinding(name);

                // Visit the associated value pattern
                Pattern ptn = !bindingPattern.fSubPattern().isNone()
                    ? buildPattern(bindingPattern.fSubPattern())
                    : null;

                // Return the result binding pattern node
                return new BindingPattern(loc(bindingPattern), this.frames.getBinding(name), ptn);
            } else {
                throw LKQLRuntimeException.fromMessage(
                    "Translation for " + pattern.getKind() + " not implemented"
                );
            }
        }

        private NodePatternDetail buildPatternDetail(Liblktlang.PatternDetail patternDetail) {
            if (patternDetail instanceof Liblktlang.FieldPatternDetail fieldPatternDetail) {
                // Translate the node pattern detail fields
                final String name = fieldPatternDetail.fId().getText();
                final Pattern expected = buildPattern(fieldPatternDetail.fExpectedValue());

                // Return the new node pattern field detail
                return NodePatternFieldNodeGen.create(loc(patternDetail), name, expected);
            } else if (patternDetail instanceof Liblktlang.PropertyPatternDetail) {
                return null;
            } else if (patternDetail instanceof Liblktlang.SelectorPatternDetail) {
                return null;
            } else {
                throw LKQLRuntimeException.fromMessage(
                    "Translation for " + patternDetail.getKind() + " not implemented"
                );
            }
        }
    }

    public static LKQLNode buildLKQLNode(Source source, LangkitRoot root, ScriptFrames frames) {
        return new TranslationPass(source, frames).buildRoot(root);
    }
}
