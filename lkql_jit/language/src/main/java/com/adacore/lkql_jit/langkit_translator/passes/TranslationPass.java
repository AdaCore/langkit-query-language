//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.exceptions.LKQLStaticErrors;
import com.adacore.lkql_jit.exceptions.LogLocation;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFrames;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.ExprArg;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.nodes.declarations.*;
import com.adacore.lkql_jit.nodes.declarations.selector.RecExprs;
import com.adacore.lkql_jit.nodes.declarations.selector.RecExprsFactory;
import com.adacore.lkql_jit.nodes.expressions.*;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyDecl;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyExpr;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockExpr;
import com.adacore.lkql_jit.nodes.expressions.dot.*;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ComprehensionAssoc;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ComprehensionAssocList;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ListComprehension;
import com.adacore.lkql_jit.nodes.expressions.literals.ObjectLiteralFactory.AtObjectValueWrapperNodeGen;
import com.adacore.lkql_jit.nodes.expressions.literals.*;
import com.adacore.lkql_jit.nodes.expressions.match.Match;
import com.adacore.lkql_jit.nodes.expressions.match.MatchArm;
import com.adacore.lkql_jit.nodes.expressions.operators.*;
import com.adacore.lkql_jit.nodes.expressions.value_read.ReadParameter;
import com.adacore.lkql_jit.nodes.expressions.value_read.ReadPrelude;
import com.adacore.lkql_jit.nodes.pass.*;
import com.adacore.lkql_jit.nodes.patterns.*;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.*;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;
import java.util.*;
import java.util.logging.Level;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/** This class contains the pass to create a Truffle tree from an LKQL parsing tree. */
public final class TranslationPass
    extends BaseTranslationPass
    implements Liblkqllang.BasicVisitor<LKQLNode> {

    // ----- Attributes -----

    /** Flag to handle some nodes differently if found inside pass declaration */
    private boolean inPass = false;

    // ----- Constructors -----

    /** Create a new LKQL translation pass. */
    public TranslationPass(
        final Source source,
        final ScriptFrames frames,
        LKQLStaticErrors errors
    ) {
        super(source, frames, errors);
    }

    /** Get the string content from a string literal node. */
    private String parseStringLiteral(final Liblkqllang.BaseStringLiteral stringLiteral) {
        // Prepare the result
        final String res;

        // If the string literal is as simple one
        if (stringLiteral instanceof Liblkqllang.StringLiteral) {
            String raw = stringLiteral.getText();
            res = StringUtils.translateEscapes(raw.substring(1, raw.length() - 1));
        }
        // If the string literal is a block iterate over all of its parts
        else {
            final StringBuilder builder = new StringBuilder();
            for (Liblkqllang.LkqlNode subBlock : ((Liblkqllang.BlockStringLiteral) stringLiteral).fDocs().children()) {
                var str = StringUtils.translateEscapes(subBlock.getText().substring(2));

                if (!str.isEmpty()) {
                    // First character should be a whitespace, as specified in
                    // the user manual.
                    if (str.charAt(0) != ' ') {
                        errors.invalidBlockStringFirstChar(loc(subBlock));
                    } else {
                        builder.append(str.substring(1)).append("\n");
                    }
                } else {
                    builder.append("\n");
                }
            }
            res = builder.toString().trim();
        }

        // Return the result
        return res;
    }

    /** Util function to create a stream from an iterable instance. */
    public static <T> Stream<T> toStream(Iterable<T> iterable) {
        return StreamSupport.stream(iterable.spliterator(), false);
    }

    // ----- Node translation methods -----

    // --- Top level nodes

    /** Visit a raw LKQL node. Always raise an exception. */
    @Override
    public LKQLNode visit(Liblkqllang.LkqlNode lkqlNode) {
        throw LKQLEngineException.shouldNotReachHere();
    }

    @Override
    public LKQLNode visit(Liblkqllang.DeclAnnotation declAnnotation) {
        // Translate the annotation fields
        final String name = declAnnotation.fName().getText();
        final ArgList arguments = (ArgList) declAnnotation.fArguments().accept(this);

        // Return the new declaration annotation node
        return new Annotation(loc(declAnnotation), name, arguments);
    }

    @Override
    public LKQLNode visit(Liblkqllang.ExprList exprList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.TopLevelList topLevelList) {
        // Enter the top level frame
        this.frames.enterFrame(topLevelList);

        // Initialize the top level nodes list
        final List<LKQLNode> topLevelNodes = new ArrayList<>(topLevelList.getChildrenCount() + 1);

        // Initialize a slot "follow_generics"
        final var followGenerics = "follow_generics";
        this.frames.declareBinding(followGenerics);
        final int followGenericsSlot = this.frames.getBinding(followGenerics);

        // Create an import node for stdlib
        final var fakeFollowGenericsImport = new Import(
            source.createUnavailableSection(),
            "stdlib",
            errors
        );

        // Create a decl "follow_generics" = Import(stdlib)."follow_generics"
        final var fakeAccess = DotAccessNodeGen.create(
            source.createUnavailableSection(),
            new Identifier(source.createUnavailableSection(), followGenerics),
            fakeFollowGenericsImport
        );
        final var fakeDecl = new ValueDeclaration(
            source.createUnavailableSection(),
            followGenericsSlot,
            fakeAccess
        );
        topLevelNodes.add(fakeDecl);

        // Iterate over all top level nodes and translate them
        for (Liblkqllang.LkqlNode child : topLevelList.children()) {
            topLevelNodes.add(child.accept(this));
        }

        // Exit the top level frame
        this.frames.exitFrame();

        String doc = null;

        // Look for 2nd element since first is the "follow_generics" decl
        if (topLevelNodes.size() > 1 && topLevelNodes.get(1) instanceof StringLiteral sl) {
            doc = sl.value;
        }

        // Return the top level node
        return new TopLevelList(
            loc(topLevelList),
            this.frames.getFrameDescriptor(),
            this.frames.getLocalSymbols(),
            topLevelNodes.toArray(new LKQLNode[0]),
            this.source.isInteractive(),
            doc
        );
    }

    // --- Literals

    @Override
    public LKQLNode visit(Liblkqllang.BoolLiteralTrue boolLiteralTrue) {
        return new BooleanLiteral(loc(boolLiteralTrue), true);
    }

    @Override
    public LKQLNode visit(Liblkqllang.BoolLiteralFalse boolLiteralFalse) {
        return new BooleanLiteral(loc(boolLiteralFalse), false);
    }

    @Override
    public LKQLNode visit(Liblkqllang.Identifier identifier) {
        return buildRead(identifier.getText(), loc(identifier));
    }

    @Override
    public LKQLNode visit(Liblkqllang.IntegerLiteral integerLiteral) {
        try {
            return new LongLiteral(loc(integerLiteral), Long.parseLong(integerLiteral.getText()));
        } catch (NumberFormatException e) {
            return new BigIntegerLiteral(
                loc(integerLiteral),
                new BigInteger(integerLiteral.getText())
            );
        }
    }

    @Override
    public LKQLNode visit(Liblkqllang.StringLiteral stringLiteral) {
        return new StringLiteral(loc(stringLiteral), parseStringLiteral(stringLiteral));
    }

    @Override
    public LKQLNode visit(Liblkqllang.BlockStringLiteral blockStringLiteral) {
        return new StringLiteral(loc(blockStringLiteral), parseStringLiteral(blockStringLiteral));
    }

    @Override
    public LKQLNode visit(Liblkqllang.UnitLiteral unitLiteral) {
        return new UnitLiteral(loc(unitLiteral));
    }

    @Override
    public LKQLNode visit(Liblkqllang.NullLiteral nullLiteral) {
        return new NullLiteral(loc(nullLiteral));
    }

    @Override
    public LKQLNode visit(Liblkqllang.SubBlockLiteral subBlockLiteral) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.UnpackAbsent unpackAbsent) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.UnpackPresent unpackPresent) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.SubBlockLiteralList subBlockLiteralList) {
        return null;
    }

    // --- If then else node

    @Override
    public LKQLNode visit(Liblkqllang.CondExpr condExpr) {
        // Translate the if then else fields
        final Expr condition = (Expr) condExpr.fCondition().accept(this);
        final Expr consequence = (Expr) condExpr.fThenExpr().accept(this);
        final Liblkqllang.Expr elseNode = condExpr.fElseExpr();
        Expr alternative = null;
        if (!elseNode.isNone()) {
            alternative = (Expr) elseNode.accept(this);
        }

        // Return the if then else node
        return CondExprNodeGen.create(loc(condExpr), condition, consequence, alternative);
    }

    // --- Unwrap node

    @Override
    public LKQLNode visit(Liblkqllang.Unwrap unwrap) {
        // Translate the unwrap field
        final Expr nodeExpr = (Expr) unwrap.fNodeExpr().accept(this);

        // Return the new unwrap node
        return new Unwrap(loc(unwrap), nodeExpr);
    }

    // --- Arguments

    @Override
    public LKQLNode visit(Liblkqllang.ExprArg exprArg) {
        // Translate the argument fields
        final Expr argExpr = (Expr) exprArg.fValueExpr().accept(this);

        // Return the new expression argument node
        return new ExprArg(loc(exprArg), argExpr);
    }

    @Override
    public LKQLNode visit(Liblkqllang.NamedArg namedArg) {
        // Translate the argument fields
        final String name = namedArg.fArgName().getText();
        final Expr argExpr = (Expr) namedArg.fValueExpr().accept(this);

        // Return the new named argument
        return new NamedArg(loc(namedArg), new Identifier(loc(namedArg.fArgName()), name), argExpr);
    }

    @Override
    public LKQLNode visit(Liblkqllang.SynthNamedArg synthNamedArg) {
        // For now, do the exact same thing as a simple named argument
        return this.visit((Liblkqllang.NamedArg) synthNamedArg);
    }

    @Override
    public LKQLNode visit(Liblkqllang.ArgList argList) {
        var args = Arrays.stream(argList.children())
            .map(a -> (Arg) a.accept(this))
            .toList();
        return buildArgs(args, loc(argList));
    }

    // --- Parameters

    @Override
    public LKQLNode visit(Liblkqllang.ParameterDecl parameterDecl) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ParameterDeclList parameterDeclList) {
        return null;
    }

    // --- Binary operations

    @Override
    public LKQLNode visit(Liblkqllang.BinOp binOp) {
        // Translate the binary operands
        final Expr left = (Expr) binOp.fLeft().accept(this);
        final Expr right = (Expr) binOp.fRight().accept(this);

        final SourceSection location = loc(binOp);

        // Create the binary operator by switching on the operator type
        return switch (binOp.fOp().getKind()) {
            case OP_PLUS -> BinPlusNodeGen.create(location, left, right);
            case OP_MINUS -> BinMinusNodeGen.create(location, left, right);
            case OP_MUL -> BinMulNodeGen.create(location, left, right);
            case OP_DIV -> BinDivNodeGen.create(location, left, right);
            case OP_AND -> BinAndNodeGen.create(location, left, right);
            case OP_OR -> BinOrNodeGen.create(location, left, right);
            case OP_EQ -> BinEqNodeGen.create(location, left, right);
            case OP_NEQ -> UnNotNodeGen.create(
                location,
                BinEqNodeGen.create(location, left, right)
            );
            case OP_CONCAT -> BinConcatNodeGen.create(location, left, right);
            case OP_LT -> BinLtNodeGen.create(location, left, right);
            case OP_LEQ -> BinLeqNodeGen.create(location, left, right);
            case OP_GT -> BinGtNodeGen.create(location, left, right);
            case OP_GEQ -> BinGeqNodeGen.create(location, left, right);
            default -> null;
        };
    }

    @Override
    public LKQLNode visit(Liblkqllang.ArithBinOp arithBinOp) {
        // Pass the arithmetic operation to the general binary operation visitor
        return this.visit((Liblkqllang.BinOp) arithBinOp);
    }

    @Override
    public LKQLNode visit(Liblkqllang.RelBinOp relBinOp) {
        // Pass the relational operation to the general binary operation visitor
        return this.visit((Liblkqllang.BinOp) relBinOp);
    }

    // --- Unary operations

    @Override
    public LKQLNode visit(Liblkqllang.UnOp unOp) {
        // Translate the unary operator argument
        final Expr arg = (Expr) unOp.fOperand().accept(this);
        final SourceSection location = loc(unOp);

        // Create the unary operator by switching on the operator type
        return switch (unOp.fOp().getKind()) {
            case OP_NOT -> UnNotNodeGen.create(location, arg);
            case OP_PLUS -> UnPlusNodeGen.create(location, arg);
            case OP_MINUS -> UnMinusNodeGen.create(location, arg);
            default -> null;
        };
    }

    // --- Operators

    @Override
    public LKQLNode visit(Liblkqllang.OpPlus opPlus) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.PatternDetailDelimiterColon patternDetailDelimiterColon) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.PatternDetailDelimiterIs patternDetailDelimiterIs) {
        LKQLContext.get(null)
            .getLogger()
            .log(
                Level.WARNING,
                "'is' syntax is deprecated for patterns. Please consider migrating your" +
                    " code via 'lkql refactor -r IS_TO_COLON path/to/your/rule_file.lkql'.",
                new LogLocation(new LogLocation.TruffleLocation(loc(patternDetailDelimiterIs)))
            );
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpMinus opMinus) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpMul opMul) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpDiv opDiv) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpAnd opAnd) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpOr opOr) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpEq opEq) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpNeq opNeq) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpConcat opConcat) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpLt opLt) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpLeq opLeq) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpGt opGt) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpGeq opGeq) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.OpNot opNot) {
        return null;
    }

    // --- Value declaration

    @Override
    public LKQLNode visit(Liblkqllang.ValDecl valDecl) {
        // Translate the declaration fields
        final String name = valDecl.fIdentifier().getText();
        final Expr value = (Expr) valDecl.fValue().accept(this);

        // Get the slot for the name
        this.frames.declareBinding(name);
        final int slot = this.frames.getBinding(name);

        // Return the value declaration node
        return new ValueDeclaration(loc(valDecl), slot, value);
    }

    // --- Dot accesses

    /** Util function to wrap a dot access node. */
    private static DotAccessWrapper wrapDotAccess(BaseDotAccess dotAccess) {
        return DotAccessWrapperNodeGen.create(dotAccess.getSourceSection(), dotAccess);
    }

    @Override
    public LKQLNode visit(Liblkqllang.DotAccess dotAccess) {
        // Translate the dot access fields
        final Liblkqllang.Identifier memberIdentifier = dotAccess.fMember();
        final Expr receiver = (Expr) dotAccess.fReceiver().accept(this);

        // Return the new DotAccess node
        return wrapDotAccess(
            DotAccessNodeGen.create(
                loc(dotAccess),
                new Identifier(loc(memberIdentifier), memberIdentifier.getText()),
                receiver
            )
        );
    }

    @Override
    public LKQLNode visit(Liblkqllang.SafeAccess safeAccess) {
        // Translate the dot access fields
        final Liblkqllang.Identifier memberIdentifier = safeAccess.fMember();
        final Expr receiver = (Expr) safeAccess.fReceiver().accept(this);

        // Return the new DotAccess node
        return wrapDotAccess(
            SafeDotAccessNodeGen.create(
                loc(safeAccess),
                new Identifier(loc(memberIdentifier), memberIdentifier.getText()),
                receiver
            )
        );
    }

    @Override
    public LKQLNode visit(Liblkqllang.UpperDotAccess upperDotAccess) {
        final var receiver = upperDotAccess.fReceiver();
        final var member = upperDotAccess.fMember();

        // Get the member reference in the node description
        final var nodeDescription = getNodeDescription(receiver);
        if (nodeDescription != null) {
            var fieldDescription = nodeDescription.fieldDescriptions().get(member.getText());
            if (fieldDescription != null) {
                return new MemberRefAccess(loc(upperDotAccess), fieldDescription.memberRef());
            } else {
                errors.noSuchField(loc(member));
            }
        }
        return null;
    }

    // --- In clause

    @Override
    public LKQLNode visit(Liblkqllang.InClause inClause) {
        // Translate the in clause fields
        final Expr elem = (Expr) inClause.fValueExpr().accept(this);
        final Expr indexable = (Expr) inClause.fListExpr().accept(this);

        // Return the new in node
        return InClauseNodeGen.create(loc(inClause), elem, indexable);
    }

    // --- Indexing

    @Override
    public LKQLNode visit(Liblkqllang.Indexing indexing) {
        // Translate the indexing fields
        final Expr collection = (Expr) indexing.fCollectionExpr().accept(this);
        final Expr index = (Expr) indexing.fIndexExpr().accept(this);

        // Return the indexing node
        return Indexing1BasedNodeGen.create(loc(indexing), false, collection, index);
    }

    @Override
    public LKQLNode visit(Liblkqllang.SafeIndexing safeIndexing) {
        // Translate the safe indexing fields
        final Expr collection = (Expr) safeIndexing.fCollectionExpr().accept(this);
        final Expr index = (Expr) safeIndexing.fIndexExpr().accept(this);

        // Return the indexing node with safe flag
        return Indexing1BasedNodeGen.create(loc(safeIndexing), true, collection, index);
    }

    // --- General patterns

    public LKQLNode visit(Liblkqllang.ComplexPattern complexPattern) {
        Pattern pattern = null;

        final Integer slot;
        if (complexPattern.fBinding().isNone()) {
            slot = null;
        } else {
            // Get the value of the binding
            final String name = complexPattern.fBinding().getText();

            // Get the slot of the binding
            this.frames.declareBinding(name);
            slot = this.frames.getBinding(name);
        }

        // Make simple pattern
        if (!complexPattern.fPattern().isNone()) {
            pattern = (Pattern) complexPattern.fPattern().accept(this);
        }

        // Make extended pattern
        if (complexPattern.fDetails().getChildrenCount() > 0) {
            // Get the pattern details
            final List<NodePatternDetail> details = new ArrayList<>();
            for (var detail : complexPattern.fDetails()) {
                details.add((NodePatternDetail) detail.accept(this));
            }

            pattern = new ExtendedNodePattern(
                loc(complexPattern),
                pattern,
                details.toArray(new NodePatternDetail[0])
            );
        }

        if (!complexPattern.fBinding().isNone()) {
            // Make binding pattern
            pattern = new BindingPattern(loc(complexPattern), slot, pattern);
        }

        if (!complexPattern.fPredicate().isNone()) {
            final Expr predicate = (Expr) complexPattern.fPredicate().accept(this);

            // Make filtered pattern
            pattern = new FilteredPattern(loc(complexPattern), pattern, predicate);
        }

        return pattern;
    }

    @Override
    public LKQLNode visit(Liblkqllang.BoolPatternFalse boolPatternFalse) {
        return BoolPatternNodeGen.create(loc(boolPatternFalse), false);
    }

    @Override
    public LKQLNode visit(Liblkqllang.BoolPatternTrue boolPatternTrue) {
        return BoolPatternNodeGen.create(loc(boolPatternTrue), true);
    }

    @Override
    public LKQLNode visit(Liblkqllang.IntegerPattern integerPattern) {
        try {
            return IntegerPatternNodeGen.create(
                loc(integerPattern),
                Long.parseLong(integerPattern.getText())
            );
        } catch (NumberFormatException e) {
            errors.invalidIntegerPattern(loc(integerPattern));
            return null;
        }
    }

    @Override
    public LKQLNode visit(Liblkqllang.ListPattern listPattern) {
        // Get the patterns inside the list
        final List<Pattern> listPatterns = new ArrayList<>();
        for (Liblkqllang.LkqlNode pattern : listPattern.fPatterns().children()) {
            listPatterns.add((Pattern) pattern.accept(this));
        }

        return ListPatternNodeGen.create(loc(listPattern), listPatterns.toArray(new Pattern[0]));
    }

    @Override
    public LKQLNode visit(Liblkqllang.SplatPattern splatPattern) {
        // Get the binding value name
        final String name = splatPattern.fBinding().getText();

        // Get the slot of the binding
        this.frames.declareBinding(name);
        final int slot = this.frames.getBinding(name);

        return new SplatPattern(loc(splatPattern), slot);
    }

    @Override
    public LKQLNode visit(Liblkqllang.IsClause isClause) {
        // Translate the is clause node expression
        final Expr nodeExpr = (Expr) isClause.fNodeExpr().accept(this);

        // Enter the "is" clause frame
        this.frames.enterFrame(isClause);

        // Translate the is clause pattern
        final Pattern pattern = (Pattern) isClause.fPattern().accept(this);

        // Exit the frame
        this.frames.exitFrame();

        // Return the result
        return IsClauseNodeGen.create(loc(isClause), pattern, nodeExpr);
    }

    @Override
    public LKQLNode visit(Liblkqllang.NullPattern nullPattern) {
        // Just return the null pattern node
        return new NullPattern(loc(nullPattern));
    }

    @Override
    public LKQLNode visit(Liblkqllang.ObjectPattern objectPattern) {
        // Get the association list and prepare the keys and values
        final Liblkqllang.LkqlNodeList nodeList = objectPattern.fPatterns();
        SplatPattern splat = null;
        final int assocNumber = nodeList.getChildrenCount();
        var keys = new ArrayList<String>();
        var patterns = new ArrayList<Pattern>();

        // Iterate on the object associations and get keys and patterns
        for (int i = 0; i < assocNumber; i++) {
            final var assoc = nodeList.getChild(i);
            if (assoc instanceof Liblkqllang.ObjectPatternAssoc objectPatternAssoc) {
                patterns.add((Pattern) objectPatternAssoc.fPattern().accept(this));
                keys.add(objectPatternAssoc.fName().getText());
            } else if (assoc instanceof Liblkqllang.SplatPattern splatPattern) {
                splat = (SplatPattern) this.visit(splatPattern);
            } else {
                // According to the LKQL parser, this is not possible
                throw LKQLEngineException.shouldNotReachHere();
            }
        }

        return ObjectPatternNodeGen.create(
            loc(objectPattern),
            patterns.toArray(new Pattern[0]),
            keys.toArray(new String[0]),
            splat
        );
    }

    @Override
    public LKQLNode visit(Liblkqllang.UniversalPattern universalPattern) {
        // Just return the universal pattern node
        return new UniversalPattern(loc(universalPattern));
    }

    @Override
    public LKQLNode visit(Liblkqllang.RegexPattern regexPattern) {
        // Get the regex string
        String regex = regexPattern.getText();
        regex = regex.substring(1, regex.length() - 1);

        // Return the new regex pattern node
        return RegexPatternNodeGen.create(loc(regexPattern), regex);
    }

    @Override
    public LKQLNode visit(Liblkqllang.TuplePattern tuplePattern) {
        // Get the sub-patterns inside the tuple pattern
        final List<Pattern> tuplePatterns = new ArrayList<>();
        for (Liblkqllang.LkqlNode pattern : tuplePattern.fPatterns().children()) {
            tuplePatterns.add((Pattern) pattern.accept(this));
        }

        return TuplePatternNodeGen.create(loc(tuplePattern), tuplePatterns.toArray(new Pattern[0]));
    }

    @Override
    public LKQLNode visit(Liblkqllang.ParenPattern parenPattern) {
        // Just return the translation of the inner pattern
        return parenPattern.fPattern().accept(this);
    }

    @Override
    public LKQLNode visit(Liblkqllang.OrPattern orPattern) {
        // Translate the or pattern fields
        final Pattern left = (Pattern) orPattern.fLeft().accept(this);
        final Pattern right = (Pattern) orPattern.fRight().accept(this);

        // Create the new or pattern node
        return new OrPattern(loc(orPattern), left, right);
    }

    @Override
    public LKQLNode visit(Liblkqllang.NotPattern notPattern) {
        // Translate the not pattern fields
        final Pattern pattern = (Pattern) notPattern.fPattern().accept(this);

        // Create the new not pattern
        return new NotPattern(loc(notPattern), pattern);
    }

    // --- Query

    @Override
    public LKQLNode visit(Liblkqllang.QueryKindAll queryKindAll) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.QueryKindFirst queryKindFirst) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.Query query) {
        // Enter the query frame
        this.frames.enterFrame(query);

        // Translate the query fields
        final Liblkqllang.Expr throughExprBase = query.fThroughExpr();
        final Expr throughExpr = throughExprBase.isNone()
            ? new ReadPrelude(loc(query), this.frames.getPrelude("subtree"))
            : (Expr) throughExprBase.accept(this);

        final Liblkqllang.Expr fromExprBase = query.fFromExpr();
        final Expr fromExpr = fromExprBase.isNone() ? null : (Expr) fromExprBase.accept(this);

        final Pattern pattern = (Pattern) query.fPattern().accept(this);

        // Get the query kind
        final Query.Kind queryKind = query.fQueryKind() instanceof Liblkqllang.QueryKindFirst
            ? Query.Kind.FIRST
            : Query.Kind.ALL;

        // Exit the query frame
        this.frames.exitFrame();

        // Return the new query node
        return new Query(loc(query), queryKind, throughExpr, fromExpr, pattern);
    }

    @Override
    public LKQLNode visit(Liblkqllang.RecExpr recExpr) {
        if (recExpr.fResultExpr().isNone()) {
            return RecExprsFactory.UnaryRecExprNodeGen.create(
                loc(recExpr),
                recExpr.fRecurseUnpack() instanceof Liblkqllang.UnpackPresent,
                (Expr) recExpr.fRecurseExpr().accept(this)
            );
        } else {
            return new RecExprs.BinaryRecExpr(
                loc(recExpr),
                recExpr.fRecurseUnpack() instanceof Liblkqllang.UnpackPresent,
                (Expr) recExpr.fRecurseExpr().accept(this),
                recExpr.fResultUnpack() instanceof Liblkqllang.UnpackPresent,
                (Expr) recExpr.fResultExpr().accept(this)
            );
        }
    }

    // --- Lists

    @Override
    public LKQLNode visit(Liblkqllang.ListLiteral listLiteral) {
        // Create the expressions of the list
        final List<Expr> listExprs = new ArrayList<>();
        for (Liblkqllang.LkqlNode expr : listLiteral.fExprs().children()) {
            listExprs.add((Expr) expr.accept(this));
        }

        // Return the list literal node
        return new ListLiteral(loc(listLiteral), listExprs.toArray(new Expr[0]));
    }

    @Override
    public LKQLNode visit(Liblkqllang.ListCompAssoc listCompAssoc) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ListCompAssocList listCompAssocList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.LkqlNodeList lkqlNodeList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ListComprehension listComprehension) {
        // Translate the list comprehension collections
        final List<Expr> collections = new ArrayList<>();
        final Liblkqllang.ListCompAssocList assocListBase = listComprehension.fGenerators();
        for (int i = 0; i < assocListBase.getChildrenCount(); i++) {
            Liblkqllang.ListCompAssoc assocOrigin =
                (Liblkqllang.ListCompAssoc) assocListBase.getChild(i);
            collections.add((Expr) assocOrigin.fCollExpr().accept(this));
        }

        // Enter the list comprehension frame
        this.frames.enterFrame(listComprehension);

        // Create the list comprehension associations
        final List<ComprehensionAssoc> assocList = new ArrayList<>();
        for (int i = 0; i < assocListBase.getChildrenCount(); i++) {
            final Liblkqllang.ListCompAssoc assocBase =
                (Liblkqllang.ListCompAssoc) assocListBase.getChild(i);
            final String name = assocBase.fBindingName().getText();
            int slot = this.frames.getParameter(name);

            assocList.add(new ComprehensionAssoc(loc(assocBase), name, slot, collections.get(i)));
        }

        // Translate the list comprehension fields
        final Expr expr = (Expr) listComprehension.fExpr().accept(this);
        final Liblkqllang.Expr guardBase = listComprehension.fGuard();
        final Expr guard = guardBase.isNone() ? null : (Expr) guardBase.accept(this);

        // Create the result
        final ListComprehension res = new ListComprehension(
            loc(listComprehension),
            this.frames.getFrameDescriptor(),
            this.frames.getClosureDescriptor(),
            new ComprehensionAssocList(
                loc(assocListBase),
                assocList.toArray(new ComprehensionAssoc[0])
            ),
            expr,
            guard
        );

        // Exit the frame
        this.frames.exitFrame();

        // Return the result
        return res;
    }

    // --- Objects

    @Override
    public LKQLNode visit(Liblkqllang.ObjectAssoc objectAssoc) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ObjectPatternAssoc objectPatternAssoc) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ObjectAssocList objectAssocList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ObjectLiteral objectLiteral) {
        // Get the association list and prepare the keys and values
        final Liblkqllang.ObjectAssocList assocList = objectLiteral.fAssocs();
        final int assocNumber = assocList.getChildrenCount();
        final List<String> keys = new ArrayList<>(assocNumber);
        final Expr[] values = new Expr[assocNumber];

        // Iterate on the object associations and get keys and values
        for (int i = 0; i < assocNumber; i++) {
            final Liblkqllang.ObjectAssoc assoc = (Liblkqllang.ObjectAssoc) assocList.getChild(i);
            final String key = assoc.fName().getText().toLowerCase();
            if (keys.contains(key)) {
                errors.multipleSameNameKeys(key, loc(objectLiteral));
            }
            keys.add(key);
            values[i] = (Expr) assoc.fExpr().accept(this);
        }

        // Return the new object literal
        return new ObjectLiteral(loc(objectLiteral), keys.toArray(new String[0]), values);
    }

    @Override
    public LKQLNode visit(Liblkqllang.AtObjectAssoc atObjectAssoc) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.AtObjectAssocList atObjectAssocList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.BasePatternList basePatternList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.AtObjectLiteral atObjectLiteral) {
        // Get the association list and prepare the keys and values
        final Liblkqllang.AtObjectAssocList assocList = atObjectLiteral.fAssocs();
        final int assocNumber = assocList.getChildrenCount();
        final List<String> keys = new ArrayList<>(assocNumber);
        final Expr[] values = new Expr[assocNumber];

        // Iterate on object associations and translate them
        for (int i = 0; i < assocNumber; i++) {
            final Liblkqllang.AtObjectAssoc assoc = (Liblkqllang.AtObjectAssoc) assocList.getChild(
                i
            );
            final String key = assoc.fName().getText().toLowerCase();
            if (keys.contains(key)) {
                errors.multipleSameNameKeys(key, loc(atObjectLiteral));
            }
            keys.add(key);
            final Liblkqllang.Expr exprBase = assoc.fExpr();
            values[i] = exprBase.isNone()
                ? new ListLiteral(
                      loc(assoc.fName()),
                      new Expr[] {
                          new ObjectLiteral(loc(assoc.fName()), new String[0], new Expr[0]),
                      }
                  )
                : AtObjectValueWrapperNodeGen.create(loc(exprBase), (Expr) exprBase.accept(this));
        }

        // Return the new object literal node because at object is juste syntactic sugar
        return new ObjectLiteral(loc(atObjectLiteral), keys.toArray(new String[0]), values);
    }

    // --- Block expressions

    @Override
    public LKQLNode visit(Liblkqllang.BlockBodyDecl blockBodyDecl) {
        // Translate the block body fields
        final Declaration declaration = (Declaration) blockBodyDecl.fDecl().accept(this);

        // Return the new block body declaration node
        return new BlockBodyDecl(loc(blockBodyDecl), declaration);
    }

    @Override
    public LKQLNode visit(Liblkqllang.BlockBodyExpr blockBodyExpr) {
        // Translate the block body fields
        Expr expr = (Expr) blockBodyExpr.fExpr().accept(this);

        // Return the new block body expression node
        return new BlockBodyExpr(loc(blockBodyExpr), expr);
    }

    @Override
    public LKQLNode visit(Liblkqllang.BlockBodyList blockBodyList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.BlockExpr blockExpr) {
        // Enter the block expression frame
        this.frames.enterFrame(blockExpr);

        // Create the declaration list
        final List<BlockBody> blockBody = new ArrayList<>();
        for (Liblkqllang.LkqlNode bodyPart : blockExpr.fBody().children()) {
            blockBody.add((BlockBody) bodyPart.accept(this));
        }

        // Get the expression of the block
        final Expr expr = (Expr) blockExpr.fExpr().accept(this);

        // Exit the block expression frame
        this.frames.exitFrame();

        // Return the result
        return new BlockExpr(loc(blockExpr), blockBody.toArray(new BlockBody[0]), expr);
    }

    // --- Function declarations

    @Override
    public LKQLNode visit(Liblkqllang.NamedFunction namedFunction) {
        return this.functionExprHelper(
            namedFunction,
            ((Liblkqllang.FunDecl) namedFunction.parent()).fName().getText()
        );
    }

    @Override
    public LKQLNode visit(Liblkqllang.AnonymousFunction anonymousFunction) {
        return this.functionExprHelper(anonymousFunction, "<anonymous>");
    }

    private FunExpr functionExprHelper(Liblkqllang.BaseFunction baseFunction, String name) {
        // Enter the function frame
        this.frames.enterFrame(baseFunction);

        var names = toStream(baseFunction.fParameters())
            .map(p -> p.fParamIdentifier().getText())
            .toArray(String[]::new);
        var types = toStream(baseFunction.fParameters())
            .map(p -> p.fTypeAnnotation().isNone() ? null : p.fTypeAnnotation().getText())
            .toArray(String[]::new);
        var defaultVals = toStream(baseFunction.fParameters())
            .map(p -> p.fDefaultExpr().isNone() ? null : (Expr) p.fDefaultExpr().accept(this))
            .toArray(Expr[]::new);
        var body = (Expr) baseFunction.fBodyExpr().accept(this);
        var docstring = baseFunction.pDoc();

        // Return the new function expression node
        var res = new FunExpr(
            loc(baseFunction),
            this.frames.getFrameDescriptor(),
            this.frames.getClosureDescriptor(),
            names,
            types,
            defaultVals,
            docstring.isNone() ? "" : parseStringLiteral(docstring),
            body,
            name
        );

        // Exit the function frame
        this.frames.exitFrame();

        // Return the result
        return res;
    }

    @Override
    public LKQLNode visit(Liblkqllang.FunDecl funDecl) {
        // Get the current slot of the name
        final String name = funDecl.fName().getText();
        this.frames.declareBinding(name);
        final int slot = this.frames.getBinding(name);

        // Translate the declaration annotation
        final Liblkqllang.DeclAnnotation annotationBase = funDecl.fAnnotation();
        final Annotation annotation = annotationBase.isNone()
            ? null
            : (Annotation) annotationBase.accept(this);

        // Translate the function body
        FunExpr funExpr = (FunExpr) funDecl.fFunExpr().accept(this);

        // Create the new function declaration node and return it
        return new FunctionDeclaration(loc(funDecl), annotation, slot, funExpr);
    }

    // --- Safe tokens

    @Override
    public LKQLNode visit(Liblkqllang.SafeAbsent safeAbsent) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.SafePresent safePresent) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorArm selectorArm) {
        return null;
    }

    // --- Function calling

    @Override
    public LKQLNode visit(Liblkqllang.FunCall funCall) {
        // Translate the function call fields
        final Expr callee = (Expr) funCall.fName().accept(this);
        final boolean isSafe = funCall.fHasSafe() instanceof Liblkqllang.SafePresent;
        final ArgList arguments = (ArgList) funCall.fArguments().accept(this);

        // Return the function call
        return FunCallNodeGen.create(
            loc(funCall),
            isSafe,
            Arrays.stream(arguments.getArgs()).map(Arg::getArgExpr).toArray(Expr[]::new),
            Arrays.stream(arguments.getArgs()).map(Arg::getArgStringName).toArray(String[]::new),
            callee
        );
    }

    /**
     * Visit a constructor call node. For now constructor is only available for rewriting nodes,
     * thus we can statically determine required children and their order.
     *
     * If the node is encountered while parsing a 'pass', it is instead instantiated as a dynamic
     * constructor call (creating a dynamically typed node).
     */
    @Override
    public LKQLNode visit(Liblkqllang.ConstructorCall constructorCall) {
        // Get the constructor argument list
        var argumentList = (ArgList) constructorCall.fArguments().accept(this);

        // Constructors aren't the same in passes and standard mode
        if (inPass) {
            // Check that all argument are named
            for (var arg : argumentList.getArgs()) {
                if (!(arg instanceof NamedArg)) errors.addDiag(
                    "constructors in passes only accept named args",
                    arg.getSourceSection()
                );
            }
            return new DynamicConstructorCall(
                loc(constructorCall),
                constructorCall.fName().getText(),
                argumentList
            );
        } else {
            // Get the node kind
            var nodeDescription = getNodeDescription(constructorCall.fName());
            if (nodeDescription != null) {
                return new ConstructorCall(
                    loc(constructorCall),
                    new Identifier(loc(constructorCall.fName()), constructorCall.fName().getText()),
                    nodeDescription,
                    argumentList,
                    errors
                );
            } else {
                return null;
            }
        }
    }

    // --- Selector declaration

    @Override
    public LKQLNode visit(Liblkqllang.SelectorArmList selectorArmList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorDecl selectorDecl) {
        // Get the name, documentation and annotation for the selector declaration
        final String name = selectorDecl.fName().getText();
        final Liblkqllang.BaseStringLiteral documentationBase = selectorDecl.fDocNode();
        final String documentation = documentationBase.isNone()
            ? ""
            : parseStringLiteral(documentationBase);

        // Get the current slot to place the new selector in
        this.frames.declareBinding(name);
        final int slot = this.frames.getBinding(name);

        // Translate the declaration annotation
        final Liblkqllang.DeclAnnotation annotationBase = selectorDecl.fAnnotation();
        final Annotation annotation = annotationBase.isNone()
            ? null
            : (Annotation) annotationBase.accept(this);

        // Enter the selector frame
        this.frames.enterFrame(selectorDecl);

        // Get the "this" binding
        final int thisSlot = this.frames.getParameter(Constants.THIS_SYMBOL);

        var arms = toStream(selectorDecl.fArms())
            .map(a -> {
                // Enter the arm's frame
                this.frames.enterFrame(a);

                // Translate the arm's fields
                final Pattern pattern = (Pattern) a.fPattern().accept(this);
                final Expr expr = (Expr) a.fExpr().accept(this);

                // Exit the arm's frame
                this.frames.exitFrame();

                // Return the new match arm
                return new MatchArm(loc(a), pattern, expr);
            })
            .toArray(MatchArm[]::new);

        var match = new Match(
            loc(selectorDecl),
            new ReadParameter(loc(selectorDecl), thisSlot),
            arms
        );

        // Create the frame and closure descriptors for the arms
        var frameDescriptor = this.frames.getFrameDescriptor();
        var closureDescriptor = this.frames.getClosureDescriptor();

        // Exit the selector frame
        this.frames.exitFrame();

        // Return the new selector declaration node
        final var minus_one = new LongLiteral(null, -1);
        final var paramNames = new String[] { "this", "depth", "min_depth", "max_depth" };
        final var paramTypes = new String[paramNames.length];
        final var paramDefaults = new Expr[] { null, minus_one, minus_one, minus_one };

        var selectorBody = new SelectorExpr(
            loc(selectorDecl),
            frameDescriptor,
            match,
            name,
            annotation != null && annotation.getName().equals(Constants.ANNOTATION_MEMOIZED)
        );
        var selectorFunction = new FunExpr(
            loc(selectorDecl),
            frameDescriptor,
            closureDescriptor,
            paramNames,
            paramTypes,
            paramDefaults,
            documentation,
            selectorBody,
            name
        );
        return new FunctionDeclaration(loc(selectorDecl), annotation, slot, selectorFunction);
    }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorCall selectorCall) {
        // Translate the selector call fields
        final SelectorCall.Quantifier quantifier;
        if (selectorCall.fQuantifier().getText().equals("any")) {
            quantifier = SelectorCall.Quantifier.ANY;
        } else {
            quantifier = SelectorCall.Quantifier.ALL;
        }

        final Liblkqllang.Expr selectorExprBase = selectorCall.fSelectorCall();
        final Expr selectorExpr;
        final ArgList args;
        if (selectorExprBase instanceof Liblkqllang.FunCall funCall) {
            selectorExpr = (Expr) funCall.fName().accept(this);
            args = (ArgList) funCall.fArguments().accept(this);
        } else {
            selectorExpr = (Expr) selectorExprBase.accept(this);
            args = null;
        }

        // Return the new node
        return new SelectorCall(loc(selectorCall), quantifier, selectorExpr, args);
    }

    // --- Node patterns

    @Override
    public LKQLNode visit(Liblkqllang.NodeKindPattern nodeKindPattern) {
        if (inPass) {
            return new DynamicNodeKindPattern(
                loc(nodeKindPattern),
                nodeKindPattern.fKindName().getText()
            );
        } else {
            return new NodeKindPattern(
                loc(nodeKindPattern),
                getNodeClass(nodeKindPattern.fKindName())
            );
        }
    }

    @Override
    public LKQLNode visit(Liblkqllang.NodePatternField nodePatternField) {
        // Translate the node pattern detail fields
        final String name = nodePatternField.fIdentifier().getText();
        final Pattern expected = (Pattern) nodePatternField.fExpectedValue().accept(this);

        nodePatternField.fPatternDetailDelimiter().accept(this);

        // Return the new node pattern field detail
        return NodePatternFieldNodeGen.create(loc(nodePatternField), name, expected);
    }

    @Override
    public LKQLNode visit(Liblkqllang.NodePatternProperty nodePatternProperty) {
        // Translate the node pattern detail fields
        final String propertyName = nodePatternProperty.fCall().fName().getText();
        final ArgList argList = (ArgList) nodePatternProperty.fCall().fArguments().accept(this);
        final Pattern expected = (Pattern) nodePatternProperty.fExpectedValue().accept(this);

        nodePatternProperty.fPatternDetailDelimiter().accept(this);

        // Return the new node pattern property detail
        return NodePatternPropertyNodeGen.create(
            loc(nodePatternProperty),
            propertyName,
            Arrays.stream(argList.getArgs()).map(Arg::getArgExpr).toArray(Expr[]::new),
            expected
        );
    }

    @Override
    public LKQLNode visit(Liblkqllang.NodePatternSelector nodePatternSelector) {
        // Translate the node pattern selector
        final SelectorCall selectorCall = (SelectorCall) nodePatternSelector.fCall().accept(this);

        // Enter the node pattern selector frame
        this.frames.enterFrame(nodePatternSelector.fPattern());

        // Translate the node pattern selector pattern
        final Pattern pattern = (Pattern) nodePatternSelector.fPattern().accept(this);

        // Exit the node pattern selector frame
        this.frames.exitFrame();

        nodePatternSelector.fPatternDetailDelimiter().accept(this);

        // Return the new selector node pattern detail
        return new NodePatternSelector(loc(nodePatternSelector), selectorCall, pattern);
    }

    @Override
    public LKQLNode visit(Liblkqllang.NodePatternDetailList nodePatternDetailList) {
        return null;
    }

    // --- Pattern matching

    @Override
    public LKQLNode visit(Liblkqllang.MatchArm matchArm) {
        // Enter the match arm frame
        this.frames.enterFrame(matchArm);

        // Translate the match arm fields
        final Pattern pattern = (Pattern) matchArm.fPattern().accept(this);
        final Expr expr = (Expr) matchArm.fExpr().accept(this);

        // Exit the match arm frame
        this.frames.exitFrame();

        // Return the new match arm
        return new MatchArm(loc(matchArm), pattern, expr);
    }

    @Override
    public LKQLNode visit(Liblkqllang.MatchArmList matchArmList) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.Match match) {
        // Translate the match fields
        final Expr matchVal = (Expr) match.fMatchedVal().accept(this);

        // Get the match arms
        final List<MatchArm> arms = new ArrayList<>();
        for (Liblkqllang.LkqlNode node : match.fArms().children()) {
            arms.add((MatchArm) node.accept(this));
        }

        // Return a new match node
        return new Match(loc(match), matchVal, arms.toArray(new MatchArm[0]));
    }

    // --- Imports

    @Override
    public LKQLNode visit(Liblkqllang.Import anImport) {
        // Get the name of the import
        final String name = anImport.fName().getText();

        // Get the slot to place the import in
        this.frames.declareBinding(name);
        final int slot = this.frames.getBinding(name);

        // Return the import node
        return new ValueDeclaration(loc(anImport), slot, new Import(loc(anImport), name, errors));
    }

    // --- Tuples

    @Override
    public LKQLNode visit(Liblkqllang.Tuple tuple) {
        // Get the expressions inside the tuple
        final List<Expr> tupleExprs = new ArrayList<>();
        for (Liblkqllang.LkqlNode expr : tuple.fExprs().children()) {
            tupleExprs.add((Expr) expr.accept(this));
        }

        // Return the tuple literal node
        return new TupleLiteral(loc(tuple), tupleExprs.toArray(new Expr[0]));
    }

    @Override
    public LKQLNode visit(Liblkqllang.PassDecl passDecl) {
        final SourceSection location = loc(passDecl);

        var addCollector = new ArrayList<Liblkqllang.AddBlock>(1);
        var delCollector = new ArrayList<Liblkqllang.DelBlock>(1);
        var rewriteCollector = new ArrayList<Liblkqllang.RewriteBlock>(1);

        for (Liblkqllang.PassBlock block : passDecl.fBlocks()) {
            switch (block) {
                case Liblkqllang.AddBlock addBlock:
                    addCollector.add(addBlock);
                    break;
                case Liblkqllang.DelBlock delBlock:
                    delCollector.add(delBlock);
                    break;
                case Liblkqllang.RewriteBlock rewriteBlock:
                    rewriteCollector.add(rewriteBlock);
                    break;
                default:
            }
        }

        // Check pass validity
        var previousSize = errors.diagnostics.size();
        if (addCollector.size() != 1) errors.addDiag(
            "pass declaration should contain exactly one `add' block",
            loc(passDecl)
        );
        if (delCollector.size() != 1) errors.addDiag(
            "pass declaration should contain exactly one `del' block",
            loc(passDecl)
        );
        if (rewriteCollector.size() != 1) errors.addDiag(
            "pass declaration should contain exactly one `rewrite' block",
            loc(passDecl)
        );

        // If the pass isn't valid, return a null node
        if (errors.diagnostics.size() != previousSize) {
            return null;
        }

        // Register pass as fake function
        final String name = passDecl.fPassName().getText();
        frames.declareBinding(name);
        final int slot = frames.getBinding(name);
        final Liblkqllang.Identifier previousName = passDecl.fPreviousPassName();
        final Optional<Integer> previousSlot = previousName.isNone()
            ? Optional.empty()
            : Optional.of(frames.getBinding(previousName.getText()));

        // Enter pass lexical environement
        inPass = true;
        frames.enterFrame(passDecl);

        final int nbFakeArgs = Constants.PASS_FAKE_ARGS.length;
        final var readParameters = new ReadParameter[nbFakeArgs];
        for (int i = 0; i < nbFakeArgs; i++) {
            final String argName = Constants.PASS_FAKE_ARGS[i];
            readParameters[i] = new ReadParameter(
                source.createUnavailableSection(),
                frames.getParameter(argName)
            );
        }

        // Digest children of passDecl
        final var addBlock = (AddBlock) addCollector.get(0).accept(this);
        final var delBlock = (DelBlock) delCollector.get(0).accept(this);
        final var rewriteBlock = (RewriteBlock) rewriteCollector.get(0).accept(this);

        final PassExpr passExpr = PassExprNodeGen.create(
            location,
            previousSlot,
            addBlock,
            delBlock,
            rewriteBlock,
            readParameters[0]
        );

        final var docstring = passDecl.pDoc();

        final var funExpr = new FunExpr(
            location,
            frames.getFrameDescriptor(),
            frames.getClosureDescriptor(),
            Constants.PASS_FAKE_ARGS,
            new String[Constants.PASS_FAKE_ARGS.length],
            new Expr[Constants.PASS_FAKE_ARGS.length],
            docstring.isNone() ? "" : parseStringLiteral(docstring),
            passExpr,
            "<pass:" + name + ">"
        );

        // Cleanup
        inPass = false;
        this.frames.exitFrame();

        final var functionDecl = new FunctionDeclaration(location, null, slot, funExpr);

        return functionDecl;
    }

    @Override
    public LKQLNode visit(Liblkqllang.PassBlockList blocks) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.AddBlock addBlock) {
        final var prefixFields = new ArrayList<PrefixField>();
        final var classDecls = new ArrayList<ClassDecl>();
        for (var clause : addBlock.fClauses()) {
            final var translated = clause.accept(this);
            switch (translated) {
                case PrefixField prefixField:
                    prefixFields.add(prefixField);
                    break;
                case ClassDecl classDecl:
                    classDecls.add(classDecl);
                    break;
                default:
                    throw LKQLEngineException.shouldNotReachHere();
            }
        }
        return new AddBlock(loc(addBlock), classDecls, prefixFields);
    }

    @Override
    public LKQLNode visit(Liblkqllang.DelBlock delBlock) {
        final var fields = new ArrayList<PrefixField>();
        final var classes = new ArrayList<String>();
        for (var clause : delBlock.fClauses()) {
            switch (clause) {
                case Liblkqllang.DotAccess prefixField:
                    // translate dot access to prefix field
                    fields.add(
                        new PrefixField(
                            loc(prefixField),
                            prefixField.fReceiver().getText(),
                            prefixField.fMember().getText()
                        )
                    );
                    break;
                case Liblkqllang.Identifier ident:
                    classes.add(ident.getText());
                    break;
                default:
                    throw LKQLEngineException.shouldNotReachHere();
            }
        }

        return new DelBlock(loc(delBlock), classes, fields);
    }

    @Override
    public LKQLNode visit(Liblkqllang.RewriteBlock rewriteBlock) {
        final var arms = rewriteBlock.fClauses();
        var clauses = new ArrayList<MatchArm>(arms.getChildrenCount());
        for (Liblkqllang.MatchArm arm : arms) {
            clauses.add((MatchArm) arm.accept(this));
        }

        return new RewriteBlock(loc(rewriteBlock), clauses);
    }

    @Override
    public LKQLNode visit(Liblkqllang.ClassDecl classDecl) {
        final var members = classDecl.fMembers();
        var fields = new ArrayList<String>(members.getChildrenCount());
        for (var member : members) {
            fields.add(member.fFieldName().getText());
        }

        return new ClassDecl(loc(classDecl), classDecl.fName().getText(), fields);
    }

    @Override
    public LKQLNode visit(Liblkqllang.RunPass runPass) {
        final var name = runPass.fStart().getText();
        final int slot = frames.getBinding(name);
        return new RunPass(loc(runPass), slot);
    }

    @Override
    public LKQLNode visit(Liblkqllang.ClassField classField) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ClassFieldList classFields) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.PrefixField prefixField) {
        return new PrefixField(
            loc(prefixField),
            prefixField.fPrefix().getText(),
            prefixField.fFieldName().getText()
        );
    }
}
