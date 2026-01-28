//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import static com.adacore.liblktlang.Liblktlang.*;

import com.adacore.liblktlang.Liblktlang;
import com.adacore.lkql_jit.Constants;
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
import com.adacore.lkql_jit.nodes.expressions.*;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyDecl;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockExpr;
import com.adacore.lkql_jit.nodes.expressions.dot.BaseDotAccess;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessWrapperNodeGen;
import com.adacore.lkql_jit.nodes.expressions.dot.SafeDotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.literals.*;
import com.adacore.lkql_jit.nodes.expressions.match.Match;
import com.adacore.lkql_jit.nodes.expressions.match.MatchArm;
import com.adacore.lkql_jit.nodes.expressions.operators.*;
import com.adacore.lkql_jit.nodes.expressions.value_read.ReadParameter;
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
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodePatternPropertyNodeGen;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;
import java.util.*;
import java.util.stream.StreamSupport;

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

        /**
         * Return whether the provided Lkt node is an expression used as a right hand operand in
         * a stream related binary operation.
         */
        private static boolean isStreamOpRhs(LktNode node) {
            return (
                node instanceof Liblktlang.Expr &&
                node.parent() instanceof Liblktlang.BinOp binOp &&
                (binOp.fOp() instanceof OpStreamCons || binOp.fOp() instanceof OpStreamConcat) &&
                binOp.fRight().equals(node)
            );
        }

        /**
         * Whether "node" needs a frame to be introduced to contain inner bindings.
         * NB: this does not include the Query node (which needs a concrete frame)
         * because it's handled in its own separate function
         */
        private static FrameKind needsFrame(LktNode node) {
            if (
                node instanceof FunDecl ||
                node instanceof Liblktlang.LambdaExpr ||
                node instanceof Liblktlang.LangkitRoot ||
                isStreamOpRhs(node) ||
                node instanceof StructDecl // lowered into function
            ) {
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
            // Queries need special handling
            if (node instanceof Liblktlang.Query query) {
                handleQuery(query, builder);
                return;
            }

            var bindingName = getBindingName(node);
            if (bindingName != null) {
                builder.addBinding(bindingName);
            }

            if (node instanceof FunParamDecl funParamDecl) {
                builder.addParameter(funParamDecl.fSynName().getText());
            } else if (node instanceof LambdaParamDecl lambdaParamDecl) {
                builder.addParameter(lambdaParamDecl.fSynName().getText());
            } else if (node instanceof FieldDecl fieldDecl) {
                builder.addParameter(fieldDecl.fSynName().getText());
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

        /**
         * Special handling for query comprehensions
         * since the source part is not included in the concrete frame.
         */
        public static void handleQuery(Liblktlang.Query query, ScriptFramesBuilder builder) {
            recurseBuildFrames(query.fSource(), builder);
            builder.openFrame(query);
            recurseBuildFrames(query.fPattern(), builder);
            if (!query.fGuard().isNone()) recurseBuildFrames(query.fGuard(), builder);
            if (!query.fMapping().isNone()) recurseBuildFrames(query.fMapping(), builder);
            builder.closeFrame();
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
                String paramName;
                Liblktlang.Expr defaultValue;
                switch (params.getChild(i)) {
                    case FunParamDecl funParamDecl -> {
                        paramName = funParamDecl.fSynName().getText();
                        defaultValue = funParamDecl.fDefaultVal();
                    }
                    case LambdaParamDecl lambdaParamDecl -> {
                        paramName = lambdaParamDecl.fSynName().getText();
                        defaultValue = lambdaParamDecl.fDefaultVal();
                    }
                    default -> throw LKQLRuntimeException.create(
                        "Cannot handle " + params.getChild(i) + " parameter declaration"
                    );
                }
                paramNames[i] = paramName;
                defaultVals[i] = defaultValue.isNone() ? null : buildExpr(defaultValue);
            }
            final var body = buildExpr(functionBody);

            final FunExpr res = new FunExpr(
                loc(functionBody),
                this.frames.getFrameDescriptor(),
                this.frames.getClosureDescriptor(),
                paramNames,
                defaultVals,
                doc == null || doc.isNone() ? "" : doc.pDenotedValue().value,
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

        private ArgList buildArgs(Liblktlang.ArgumentList args) {
            return buildArgs(
                StreamSupport.stream(args.spliterator(), false).map(this::buildArg).toList(),
                loc(args)
            );
        }

        private Annotation buildAnnotation(Liblktlang.DeclAnnotation annotation) {
            return new Annotation(
                loc(annotation),
                annotation.fName().getText(),
                buildArgs(annotation.fArgs().fArgs())
            );
        }

        private Declaration buildDecl(Liblktlang.Decl decl) {
            return switch (decl) {
                case Liblktlang.FunDecl funDecl -> {
                    final var name = funDecl.fSynName().getText();
                    frames.declareBinding(name);
                    // Full decl
                    final var fullDecl = (FullDecl) funDecl.parent();

                    // Annotation
                    final var annotations = fullDecl.fDeclAnnotations();
                    final var annotation = annotations.getChildrenCount() > 0
                        ? buildAnnotation((Liblktlang.DeclAnnotation) annotations.getChild(0))
                        : null;

                    final var ret = new FunctionDeclaration(
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

                    if (annotation == null) yield ret;

                    yield switch (annotation.getName()) {
                        case Constants.ANNOTATION_NODE_CHECK -> new CheckerExport(
                            loc(funDecl),
                            annotation,
                            CheckerExport.CheckerMode.NODE,
                            ret
                        );
                        case Constants.ANNOTATION_UNIT_CHECK -> new CheckerExport(
                            loc(funDecl),
                            annotation,
                            CheckerExport.CheckerMode.UNIT,
                            ret
                        );
                        default -> ret;
                    };
                }
                case Liblktlang.ValDecl valDecl -> {
                    final var name = valDecl.fSynName().getText();
                    // Build expr BEFORE the name is bound
                    final var value = buildExpr(valDecl.fExpr());
                    frames.declareBinding(name);
                    yield new ValueDeclaration(loc(valDecl), frames.getBinding(name), value);
                }
                case Liblktlang.StructDecl structDecl -> {
                    // Structs declarations are lowered to a constructor
                    final var name = structDecl.fSynName().getText();
                    frames.declareBinding(name);
                    final var slot = frames.getBinding(name);

                    final var fullDecl = (FullDecl) structDecl.parent();
                    final var doc = fullDecl.fDoc();

                    // Build constructor formal parameters
                    final var params = new String[structDecl.fDecls().getChildrenCount()];
                    final var defaultVals = new Expr[params.length];

                    final var localDecls = structDecl.fDecls().iterator();
                    for (int i = 0; i < params.length; i++) {
                        final var localDecl = localDecls.next().fDecl();

                        if (localDecl instanceof Liblktlang.FieldDecl fieldDecl) {
                            params[i] = fieldDecl.fSynName().getText();
                            if (!fieldDecl.fDefaultVal().isNone()) {
                                defaultVals[i] = buildExpr(fieldDecl.fDefaultVal());
                            }
                        } else throw translationError(
                            localDecl,
                            "Unexpected declaration inside a struct"
                        );
                    }

                    frames.enterFrame(structDecl);

                    // Build body

                    final var keys = new String[params.length + 1];
                    final var vals = new Expr[params.length + 1];

                    for (int i = 0; i < params.length; i++) {
                        frames.declareBinding(params[i]);
                        keys[i] = params[i];
                        vals[i] = new ReadParameter(
                            loc(structDecl.fDecls().getChild(i)),
                            frames.getParameter(params[i])
                        );
                    }

                    keys[keys.length - 1] = Constants.STRUCT_TYPE_TAG;
                    vals[vals.length - 1] = new StringLiteral(loc(structDecl.fSynName()), name);

                    final var body = new ObjectLiteral(loc(structDecl), keys, vals);

                    final var res = new FunctionDeclaration(
                        loc(structDecl),
                        null, // no annotation supported for structs
                        slot,
                        new FunExpr(
                            loc(structDecl),
                            frames.getFrameDescriptor(),
                            frames.getClosureDescriptor(),
                            params,
                            defaultVals,
                            doc.isNone() ? "" : doc.pDenotedValue().value,
                            body,
                            name
                        )
                    );

                    frames.exitFrame();
                    yield res;
                }
                default -> {
                    throw LKQLRuntimeException.create(
                        "Translation for " + decl.getKind() + " not implemented"
                    );
                }
            };
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
            } else if (expr instanceof Liblktlang.ParenExpr parenExpr) {
                return buildExpr(parenExpr.fExpr());
            } else if (expr instanceof CallExpr callExpr) {
                final Liblktlang.Expr calleeNode = callExpr.fName();

                // Special case for the "Unit()" call. This is the constructor for the "Unit" type
                // in the Lkt language.
                if (calleeNode.getText().equals("Unit")) {
                    return new UnitLiteral(loc(callExpr));
                }

                // In all other cases, translate the call expression to a function call
                final Expr callee = buildExpr(callExpr.fName());
                final ArgList arguments = buildArgs(callExpr.fArgs());
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
            } else if (expr instanceof Liblktlang.ArrayLiteral arrayLiteral) {
                return new ListLiteral(
                    loc(arrayLiteral),
                    Arrays.stream(arrayLiteral.fExprs().children())
                        .map(e -> buildExpr((Liblktlang.Expr) e))
                        .toList()
                        .toArray(new Expr[0])
                );
            } else if (
                expr instanceof Liblktlang.BinOp binOp &&
                (binOp.fOp() instanceof OpStreamCons || binOp.fOp() instanceof OpStreamConcat)
            ) {
                // Special case for stream constructors since the right operand is lazy
                final var head = buildExpr(binOp.fLeft());
                this.frames.enterFrame(binOp.fRight());
                final var tail = buildExpr(binOp.fRight());

                final var res = switch (binOp.fOp().getKind()) {
                    case OP_STREAM_CONS -> StreamConsNodeGen.create(
                        loc(binOp),
                        tail,
                        this.frames.getFrameDescriptor(),
                        this.frames.getClosureDescriptor(),
                        head
                    );
                    case OP_STREAM_CONCAT -> StreamConcatNodeGen.create(
                        loc(binOp),
                        tail,
                        this.frames.getFrameDescriptor(),
                        this.frames.getClosureDescriptor(),
                        head
                    );
                    default -> null;
                };

                this.frames.exitFrame();
                return res;
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
            } else if (expr instanceof SubscriptExpr subscriptExpr) {
                return IndexingNodeGen.create(
                    loc(subscriptExpr),
                    subscriptExpr.fNullCond() instanceof NullCondQualifierPresent,
                    buildExpr(subscriptExpr.fPrefix()),
                    buildExpr(subscriptExpr.fIndex())
                );
            } else if (expr instanceof DotExpr dotExpr) {
                final var memberIdentifier = dotExpr.fSuffix();
                final var loc = loc(dotExpr);
                final var id = new Identifier(loc(memberIdentifier), memberIdentifier.getText());
                final var prefix = buildExpr(dotExpr.fPrefix());
                final BaseDotAccess dotAccess = (dotExpr.fNullCond() instanceof
                            Liblktlang.NullCondQualifierPresent)
                    ? SafeDotAccessNodeGen.create(loc, id, prefix)
                    : DotAccessNodeGen.create(loc, id, prefix);
                return DotAccessWrapperNodeGen.create(dotAccess.getSourceSection(), dotAccess);
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
            } else if (expr instanceof Liblktlang.LambdaExpr lambdaExpr) {
                return createFunExpr(
                    lambdaExpr,
                    "<anonymous>",
                    null,
                    lambdaExpr.fBody(),
                    lambdaExpr.fParams()
                );
            } else if (expr instanceof Liblktlang.Query query) {
                var source = buildExpr(query.fSource());

                this.frames.enterFrame(query);

                var pattern = buildPattern(query.fPattern());
                var guard = query.fGuard().isNone() ? null : buildExpr(query.fGuard());
                var result = query.fMapping().isNone() ? null : buildExpr(query.fMapping());

                var frameDescriptor = this.frames.getFrameDescriptor();
                var closureDescriptor = this.frames.getClosureDescriptor();

                this.frames.exitFrame();

                return QueryComprehensionNodeGen.create(
                    loc(query),
                    frameDescriptor,
                    closureDescriptor,
                    pattern,
                    guard,
                    result,
                    source
                );
            } else {
                throw LKQLRuntimeException.create(
                    "Translation for " + expr.getKind() + " not implemented"
                );
            }
        }

        private Pattern buildPattern(Liblktlang.Pattern pattern) {
            return switch (pattern) {
                case Liblktlang.ComplexPattern complexPattern:
                    Pattern result = null;

                    final Integer slot;
                    if (complexPattern.fDecl().isNone()) {
                        slot = null;
                    } else {
                        // Get the value of the binding
                        final String name = complexPattern.fDecl().fSynName().getText();
                        this.frames.declareBinding(name);
                        slot = this.frames.getBinding(name);
                    }

                    // Make simple pattern
                    if (!complexPattern.fPattern().isNone()) {
                        result = buildPattern(complexPattern.fPattern());
                    }

                    // Make extended pattern
                    if (complexPattern.fDetails().getChildrenCount() > 0) {
                        // Get the pattern details
                        final List<NodePatternDetail> details = new ArrayList<>();
                        for (var detail : complexPattern.fDetails()) {
                            details.add(buildPatternDetail(detail));
                        }

                        result = new ExtendedNodePattern(
                            loc(complexPattern),
                            result,
                            details.toArray(new NodePatternDetail[0])
                        );
                    }

                    if (!complexPattern.fDecl().isNone()) {
                        // Make binding pattern
                        result = new BindingPattern(loc(complexPattern), slot, result);
                    }

                    if (!complexPattern.fPredicate().isNone()) {
                        // Make filtered pattern
                        result = new FilteredPattern(
                            loc(complexPattern),
                            result,
                            buildExpr(complexPattern.fPredicate())
                        );
                    }

                    yield result;
                case Liblktlang.NullPattern nullPattern:
                    yield new NullPattern(loc(nullPattern));
                case Liblktlang.IntegerPattern integerPattern:
                    try {
                        yield IntegerPatternNodeGen.create(
                            loc(integerPattern),
                            Integer.parseInt(integerPattern.getText())
                        );
                    } catch (NumberFormatException e) {
                        throw translationError(
                            integerPattern,
                            "Invalid number literal for pattern"
                        );
                    }
                case Liblktlang.TypePattern typePattern:
                    yield new NodeKindPattern(loc(typePattern), typePattern.fTypeName().getText());
                case Liblktlang.AnyTypePattern univPattern:
                    yield new UniversalPattern(loc(univPattern));
                case Liblktlang.RegexPattern regexPattern:
                    var regex = regexPattern.getText();
                    regex = regex.substring(1, regex.length() - 1);
                    yield RegexPatternNodeGen.create(loc(regexPattern), regex);
                case Liblktlang.ParenPattern parenPattern:
                    final Pattern p = buildPattern(parenPattern.fSubPattern());
                    yield new ParenPattern(loc(parenPattern), p);
                case Liblktlang.OrPattern orPattern:
                    yield new OrPattern(
                        loc(orPattern),
                        buildPattern(orPattern.fLeftSubPattern()),
                        buildPattern(orPattern.fRightSubPattern())
                    );
                default:
                    throw LKQLRuntimeException.create(
                        "Translation for " + pattern.getKind() + " not implemented"
                    );
            };
        }

        private NodePatternDetail buildPatternDetail(Liblktlang.PatternDetail patternDetail) {
            return switch (patternDetail) {
                case Liblktlang.FieldPatternDetail fieldPatternDetail: {
                    // Translate the node pattern detail fields
                    final String name = fieldPatternDetail.fId().getText();
                    final Pattern expected = buildPattern(fieldPatternDetail.fExpectedValue());

                    // Return the new node pattern field detail
                    yield NodePatternFieldNodeGen.create(loc(patternDetail), name, expected);
                }
                case Liblktlang.PropertyPatternDetail propertyPatternDetail: {
                    // Translate the node pattern detail fields
                    final var callExpr = propertyPatternDetail.fCall();
                    final String propertyName = callExpr.fName().getText();
                    final ArgList argList = buildArgs(callExpr.fArgs());
                    final Pattern expected = buildPattern(propertyPatternDetail.fExpectedValue());

                    // Return the new node pattern property detail
                    yield NodePatternPropertyNodeGen.create(
                        loc(propertyPatternDetail),
                        propertyName,
                        Arrays.stream(argList.getArgs()).map(Arg::getArgExpr).toArray(Expr[]::new),
                        expected
                    );
                }
                default:
                    throw new AssertionError("Unreachable");
            };
        }
    }

    public static LKQLNode buildLKQLNode(Source source, LangkitRoot root, ScriptFrames frames) {
        return new TranslationPass(source, frames).buildRoot(root);
    }
}
