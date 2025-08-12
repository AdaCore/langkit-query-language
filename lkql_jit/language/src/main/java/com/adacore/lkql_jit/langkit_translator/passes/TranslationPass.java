//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import static com.adacore.lkql_jit.options.IterationUtils.toStream;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.TranslatorException;
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
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorDeclaration;
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
import com.adacore.lkql_jit.nodes.expressions.value_read.*;
import com.adacore.lkql_jit.nodes.pass.*;
import com.adacore.lkql_jit.nodes.patterns.*;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.*;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;
import java.util.*;

/**
 * This class represents the translation pass to get a Truffle AST from a Langkit AST.
 *
 * @author Hugo GUERRIER
 */
public final class TranslationPass implements Liblkqllang.BasicVisitor<LKQLNode> {

    // ----- Attributes -----

    /** The source of the AST to translate. */
    private final Source source;

    /** The frame descriptions for the LKQL script. */
    private final ScriptFrames scriptFrames;

    /** Flag to handle some nodes differently if found inside pass declaration */
    private boolean inPass = false;

    // ----- Constructors -----

    /**
     * Create a new translation pass.
     *
     * @param source The source of the AST to translate.
     * @param scriptFrames The descriptions of the script frames.
     */
    public TranslationPass(final Source source, final ScriptFrames scriptFrames) {
        this.source = source;
        this.scriptFrames = scriptFrames;
    }

    // ----- Instance methods -----

    /**
     * Create the source location for the given node.
     *
     * @param node The node to create the source location for.
     * @return The source location.
     */
    private SourceSection loc(final Liblkqllang.LkqlNode node) {
        return SourceSectionWrapper.createSection(node.getSourceLocationRange(), this.source);
    }

    private RuntimeException translationError(Liblkqllang.LkqlNode node, String message) {
        var ctx = LKQLLanguage.getContext(null);
        ctx
            .getDiagnosticEmitter()
            .emitDiagnostic(
                CheckerUtils.MessageKind.ERROR,
                message,
                null,
                SourceSectionWrapper.create(node.getSourceLocationRange(), source)
            );
        return LKQLRuntimeException.fromMessage("Errors during analysis");
    }

    private RuntimeException multipleSameNameKeys(Liblkqllang.LkqlNode node, String key) {
        throw translationError(
            node,
            "Multiple keys with the same name in the object: \"" + key + "\""
        );
    }

    /**
     * Get the string content from a string literal node.
     *
     * @param stringLiteral The string literal node.
     * @return The string content of it.
     */
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
            for (Liblkqllang.LkqlNode subBlock : ((Liblkqllang.BlockStringLiteral) stringLiteral).fDocs()
                .children()) {
                var str = StringUtils.translateEscapes(subBlock.getText().substring(2));

                if (str.length() > 0) {
                    // First character should be a whitespace, as specified in
                    // the user manual.
                    if (str.charAt(0) != ' ') {
                        throw translationError(
                            stringLiteral,
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

    // ----- Node translation methods -----

    // --- Top level nodes

    /**
     * Visit a raw LKQL node. Always raise an exception.
     *
     * @param lkqlNode The node to visit.
     */
    @Override
    public LKQLNode visit(Liblkqllang.LkqlNode lkqlNode) {
        throw new TranslatorException("Cannot visit a raw LKQL node during the translation pass");
    }

    /**
     * Visit a declaration annotation node.
     *
     * @param declAnnotation The declaration annotation node from Langkit.
     * @return The declaration annotation node for Truffle.
     */
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

    /**
     * Visit the top level node list, this what all LKQL programs start with.
     *
     * @param topLevelList The base TopLevelList node from Langkit.
     * @return The TopLevelList node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.TopLevelList topLevelList) {
        // Enter the top level frame
        this.scriptFrames.enterFrame(topLevelList);

        // Initialize the top level nodes list
        final List<LKQLNode> topLevelNodes = new ArrayList<>();

        // Iterate over all top level nodes and translate them
        for (Liblkqllang.LkqlNode child : topLevelList.children()) {
            topLevelNodes.add(child.accept(this));
        }

        // Exit the top level frame
        this.scriptFrames.exitFrame();

        String doc = null;

        if (topLevelNodes.size() > 0 && topLevelNodes.get(0) instanceof StringLiteral) {
            doc = ((StringLiteral) topLevelNodes.get(0)).value;
        }

        // Return the top level node
        return new TopLevelList(
            loc(topLevelList),
            this.scriptFrames.getFrameDescriptor(),
            topLevelNodes.toArray(new LKQLNode[0]),
            this.source.isInteractive(),
            doc
        );
    }

    // --- Literals

    /**
     * Visit a true boolean literal node.
     *
     * @param boolLiteralTrue The boolean literal node from Langkit.
     * @return The boolean literal node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.BoolLiteralTrue boolLiteralTrue) {
        return new BooleanLiteral(loc(boolLiteralTrue), true);
    }

    /**
     * Visit a false boolean literal node.
     *
     * @param boolLiteralFalse The boolean literal node from Langkit.
     * @return The boolean literal node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.BoolLiteralFalse boolLiteralFalse) {
        return new BooleanLiteral(loc(boolLiteralFalse), false);
    }

    /**
     * Visit an identifier node.
     *
     * @param identifier The identifier node from Langkit.
     * @return The identifier node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.Identifier identifier) {
        // Get the identifier string
        final String symbol = identifier.getText();
        final SourceSection location = loc(identifier);

        // First look for the symbol in the frame local bindings
        if (this.scriptFrames.isBinding(symbol) && this.scriptFrames.isBindingDeclared(symbol)) {
            return new ReadLocal(location, this.scriptFrames.getBinding(symbol));
        }
        // In a second time look in the parameters of the frame
        else if (this.scriptFrames.isParameter(symbol)) {
            return new ReadParameter(location, this.scriptFrames.getParameter(symbol));
        }
        // Then look in the closure for the symbol
        else if (this.scriptFrames.isClosure(symbol)) {
            final var slotInfo = this.scriptFrames.getClosure(symbol);
            if (this.scriptFrames.isClosureDeclared(symbol)) {
                return ReadClosureNodeGen.create(location, slotInfo.slot(), slotInfo.isGlobal());
            } else {
                return new ReadClosureUnsafe(location, slotInfo.slot(), symbol);
            }
        } else if (this.scriptFrames.isPrelude(symbol)) {
            return new ReadPrelude(location, this.scriptFrames.getPrelude(symbol));
        }
        // Finally look in the LKQL built-ins
        else if (this.scriptFrames.isBuiltIn(symbol)) {
            return new ReadBuiltIn(location, this.scriptFrames.getBuiltIn(symbol));
        }
        // If we're in interactive mode and the symbol hasn't been found any other way, issue a
        // ReadDynamic, which will read from the global scope. This is only necessary in
        // interactive mode.
        else if (this.source.isInteractive()) {
            return new ReadDynamic(location, symbol);
        }

        // If the symbol hasn't been found, throw an exception
        throw translationError(identifier, "Unknown symbol: " + symbol);
    }

    /**
     * Visit a integer literal node.
     *
     * @param integerLiteral The integer literal node from Langkit.
     * @return The integer literal node for Truffle.
     */
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

    /**
     * Visit a string literal node.
     *
     * @param stringLiteral The string literal node from Langkit.
     * @return The string literal node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.StringLiteral stringLiteral) {
        return new StringLiteral(loc(stringLiteral), parseStringLiteral(stringLiteral));
    }

    /**
     * Visit a block string literal node.
     *
     * @param blockStringLiteral The block string literal node from Langkit.
     * @return The block string literal node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.BlockStringLiteral blockStringLiteral) {
        return new StringLiteral(loc(blockStringLiteral), parseStringLiteral(blockStringLiteral));
    }

    /**
     * Visit a unit literal node.
     *
     * @param unitLiteral The unit literal node from Langkit.
     * @return The unit literal node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.UnitLiteral unitLiteral) {
        return new UnitLiteral(loc(unitLiteral));
    }

    /**
     * Visit a null literal node.
     *
     * @param nullLiteral The null literal node from Langkit.
     * @return The null literal node for Truffle.
     */
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

    /**
     * Visit an "if then else" node.
     *
     * @param condExpr The "if then else" node from Langkit.
     * @return The "if then else" node for Truffle.
     */
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

    /**
     * Visit an unwrap node.
     *
     * @param unwrap The unwrap node from Langkit.
     * @return The unwrap node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.Unwrap unwrap) {
        // Translate the unwrap field
        final Expr nodeExpr = (Expr) unwrap.fNodeExpr().accept(this);

        // Return the new unwrap node
        return new Unwrap(loc(unwrap), nodeExpr);
    }

    // --- Arguments

    /**
     * Visit an expression argument node.
     *
     * @param exprArg The expression argument node from Langkit.
     * @return The expression argument node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.ExprArg exprArg) {
        // Translate the argument fields
        final Expr argExpr = (Expr) exprArg.fValueExpr().accept(this);

        // Return the new expression argument node
        return new ExprArg(loc(exprArg), argExpr);
    }

    /**
     * Visit a named argument node.
     *
     * @param namedArg The named argument node from Langkit.
     * @return The named argument node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.NamedArg namedArg) {
        // Translate the argument fields
        final String name = namedArg.fArgName().getText();
        final Expr argExpr = (Expr) namedArg.fValueExpr().accept(this);

        // Return the new named argument
        return new NamedArg(loc(namedArg), new Identifier(loc(namedArg.fArgName()), name), argExpr);
    }

    /**
     * Visit a synthetic named argument node.
     *
     * @param synthNamedArg The synthetic named argument node from Langkit.
     * @return The synthetic named argument node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.SynthNamedArg synthNamedArg) {
        // For now, do the exact same thing as a simple named argument
        return this.visit((Liblkqllang.NamedArg) synthNamedArg);
    }

    /**
     * Visit an argument list node.
     *
     * @param argList The argument list node from Langkit.
     * @return The argument list node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.ArgList argList) {
        // Prepare the argument list
        final List<Arg> arguments = new ArrayList<>();

        // Visit all argument to create the argument list and perform static checks
        boolean namedPhase = false;
        final Set<String> seenNames = new HashSet<>();
        for (Liblkqllang.LkqlNode arg : argList.children()) {
            final Arg curArg = (Arg) arg.accept(this);

            // Verify the position after named arguments
            if (curArg instanceof ExprArg) {
                if (namedPhase) {
                    throw LKQLRuntimeException.positionAfterNamedArgument(curArg);
                }
            }
            // Verify the same name arguments
            else if (curArg instanceof NamedArg namedArg) {
                namedPhase = true;
                if (seenNames.contains(namedArg.getArgStringName())) {
                    throw LKQLRuntimeException.multipleSameNameArgument(curArg);
                }
                seenNames.add(namedArg.getArgStringName());
            }

            // Add the argument to the list
            arguments.add(curArg);
        }

        // Return the new argument list node
        return new ArgList(loc(argList), arguments.toArray(new Arg[0]));
    }

    // --- Parameters

    /**
     * Visit a parameter declaration node.
     *
     * @param parameterDecl The parameter declaration node from Langkit
     * @return The parameter declaration node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ParameterDecl parameterDecl) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ParameterDeclList parameterDeclList) {
        return null;
    }

    // --- Binary operations

    /**
     * Visit a binary operation node.
     *
     * @param binOp The binary operation node from Langkit.
     * @return The binary operation node for Truffle.
     */
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
            case OP_NEQ -> BinNeqNodeGen.create(location, left, right);
            case OP_CONCAT -> BinConcatNodeGen.create(location, left, right);
            case OP_LT -> BinLtNodeGen.create(location, left, right);
            case OP_LEQ -> BinLeqNodeGen.create(location, left, right);
            case OP_GT -> BinGtNodeGen.create(location, left, right);
            case OP_GEQ -> BinGeqNodeGen.create(location, left, right);
            default -> null;
        };
    }

    /**
     * Visit an arithmetic binary operation node
     *
     * @param arithBinOp The base ArithBinOp node from Langkit
     * @return The BinOp node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ArithBinOp arithBinOp) {
        // Pass the arithmetic operation to the general binary operation visitor
        return this.visit((Liblkqllang.BinOp) arithBinOp);
    }

    /**
     * Visit a relational binary operation node.
     *
     * @param relBinOp The relational binary operation node from Langkit.
     * @return The relational binary operation node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.RelBinOp relBinOp) {
        // Pass the relational operation to the general binary operation visitor
        return this.visit((Liblkqllang.BinOp) relBinOp);
    }

    // --- Unary operations

    /**
     * Visit a unary operation node.
     *
     * @param unOp The unary operation node from Langkit.
     * @return The unary operation node for Truffle.
     */
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
        var ctx = LKQLLanguage.getContext(null);
        ctx
            .getDiagnosticEmitter()
            .emitDiagnostic(
                CheckerUtils.MessageKind.WARNING,
                "'is' syntax is deprecated for patterns. Please consider migrating your" +
                " code via 'lkql refactor -r IS_TO_COLON " +
                "path/to/your/rule_file.lkql'.",
                null,
                SourceSectionWrapper.create(
                    patternDetailDelimiterIs.getSourceLocationRange(),
                    source
                )
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

    /**
     * Visit a variable declaration node.
     *
     * @param valDecl The value declaration node from Langkit.
     * @return The value declaration node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.ValDecl valDecl) {
        // Translate the declaration fields
        final String name = valDecl.fIdentifier().getText();
        final Expr value = (Expr) valDecl.fValue().accept(this);

        // Get the slot for the name
        this.scriptFrames.declareBinding(name);
        final int slot = this.scriptFrames.getBinding(name);

        // Return the value declaration node
        return new ValueDeclaration(loc(valDecl), slot, value);
    }

    // --- Dot accesses

    /** Util function to wrap a dot access node. */
    private static DotAccessWrapper wrapDotAccess(BaseDotAccess dotAccess) {
        return DotAccessWrapperNodeGen.create(dotAccess.getSourceSection(), dotAccess);
    }

    /**
     * Visit a dot access node.
     *
     * @param dotAccess The dot access node from Langkit.
     * @return The dot access node for Truffle.
     */
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

    /**
     * Visit a safe dot access node.
     *
     * @param safeAccess The safe dot access node from Langkit.
     * @return The safe dot access node for Truffle.
     */
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

    /** Visit an upper dot-access node and translate it into a member reference access. */
    @Override
    public LKQLNode visit(Liblkqllang.UpperDotAccess upperDotAccess) {
        final var receiver = upperDotAccess.fReceiver();
        final var member = upperDotAccess.fMember();
        return new MemberRefAccess(
            loc(upperDotAccess),
            new Identifier(loc(receiver), receiver.getText()),
            new Identifier(loc(member), member.getText())
        );
    }

    // --- In clause

    /**
     * Visit a "in" clause node.
     *
     * @param inClause The "in" clause node from Langkit.
     * @return The "in" clause node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.InClause inClause) {
        // Translate the in clause fields
        final Expr elem = (Expr) inClause.fValueExpr().accept(this);
        final Expr indexable = (Expr) inClause.fListExpr().accept(this);

        // Return the new in node
        return InClauseNodeGen.create(loc(inClause), elem, indexable);
    }

    // --- Indexing

    /**
     * Visit an indexing node.
     *
     * @param indexing The indexing node from Langkit.
     * @return The indexing node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.Indexing indexing) {
        // Translate the indexing fields
        final Expr collection = (Expr) indexing.fCollectionExpr().accept(this);
        final Expr index = (Expr) indexing.fIndexExpr().accept(this);

        // Return the indexing node
        return IndexingNodeGen.create(loc(indexing), false, collection, index);
    }

    /**
     * Visit a safe indexing node.
     *
     * @param safeIndexing The safe indexing node from Langkit.
     * @return The safe indexing node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.SafeIndexing safeIndexing) {
        // Translate the safe indexing fields
        final Expr collection = (Expr) safeIndexing.fCollectionExpr().accept(this);
        final Expr index = (Expr) safeIndexing.fIndexExpr().accept(this);

        // Return the indexing node with safe flag
        return IndexingNodeGen.create(loc(safeIndexing), true, collection, index);
    }

    // --- General patterns

    /**
     * Visit a filtered pattern node.
     *
     * @param filteredPattern The filtered pattern node from Langkit.
     * @return The filtered pattern node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.FilteredPattern filteredPattern) {
        // Translate the filtered pattern fields
        final UnfilteredPattern pattern = (UnfilteredPattern) filteredPattern
            .fPattern()
            .accept(this);
        final Expr predicate = (Expr) filteredPattern.fPredicate().accept(this);

        // Create the new filtered pattern node
        return new FilteredPattern(loc(filteredPattern), pattern, predicate);
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
                Integer.parseInt(integerPattern.getText())
            );
        } catch (NumberFormatException e) {
            throw translationError(integerPattern, "Invalid number literal for pattern");
        }
    }

    @Override
    public LKQLNode visit(Liblkqllang.ListPattern listPattern) {
        // Get the patterns inside the list
        final List<BasePattern> listPatterns = new ArrayList<>();
        for (Liblkqllang.LkqlNode pattern : listPattern.fPatterns().children()) {
            listPatterns.add((BasePattern) pattern.accept(this));
        }

        return ListPatternNodeGen.create(
            loc(listPattern),
            listPatterns.toArray(new BasePattern[0])
        );
    }

    @Override
    public LKQLNode visit(Liblkqllang.SplatPattern splatPattern) {
        // Get the binding value name
        final String name = splatPattern.fBinding().getText();

        // Get the slot of the binding
        this.scriptFrames.declareBinding(name);
        final int slot = this.scriptFrames.getBinding(name);

        return new SplatPattern(loc(splatPattern), slot);
    }

    /**
     * Visit a binding pattern node.
     *
     * @param bindingPattern The binding pattern node from Langkit.
     * @return The binding pattern node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.BindingPattern bindingPattern) {
        // Get the binding value name
        final String name = bindingPattern.fBinding().getText();

        // Get the slot of the binding
        this.scriptFrames.declareBinding(name);
        final int slot = this.scriptFrames.getBinding(name);

        ValuePattern pattern = null;
        // Visit the associated value pattern
        if (!bindingPattern.fValuePattern().isNone()) {
            pattern = (ValuePattern) bindingPattern.fValuePattern().accept(this);
        }

        // Return the result binding pattern node
        return new BindingPattern(loc(bindingPattern), slot, pattern);
    }

    /**
     * Visit a "is" clause node.
     *
     * @param isClause The "is" claus node from Langkit.
     * @return The "is" clause node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.IsClause isClause) {
        // Translate the is clause node expression
        final Expr nodeExpr = (Expr) isClause.fNodeExpr().accept(this);

        // Enter the "is" clause frame
        this.scriptFrames.enterFrame(isClause);

        // Translate the is clause pattern
        final BasePattern pattern = (BasePattern) isClause.fPattern().accept(this);

        // Exit the frame
        this.scriptFrames.exitFrame();

        // Return the result
        return IsClauseNodeGen.create(loc(isClause), pattern, nodeExpr);
    }

    /**
     * Visit a null pattern node.
     *
     * @param nullPattern The null pattern node from Langkit.
     * @return The null pattern node for Truffle.
     */
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
        var patterns = new ArrayList<BasePattern>();

        // Iterate on the object associations and get keys and patterns
        for (int i = 0; i < assocNumber; i++) {
            final var assoc = nodeList.getChild(i);
            if (assoc instanceof Liblkqllang.ObjectPatternAssoc objectPatternAssoc) {
                patterns.add((BasePattern) objectPatternAssoc.fPattern().accept(this));
                keys.add(objectPatternAssoc.fName().getText());
            } else if (assoc instanceof Liblkqllang.SplatPattern splatPattern) {
                splat = (SplatPattern) this.visit(splatPattern);
            } else {
                throw translationError(objectPattern, "Invalid assoc in object pattern");
            }
        }

        return ObjectPatternNodeGen.create(
            loc(objectPattern),
            patterns.toArray(new BasePattern[0]),
            keys.toArray(new String[0]),
            splat
        );
    }

    /**
     * Visit a universal pattern node.
     *
     * @param universalPattern The universal pattern node from Langkit.
     * @return The universal pattern node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.UniversalPattern universalPattern) {
        // Just return the universal pattern node
        return new UniversalPattern(loc(universalPattern));
    }

    /**
     * Visit a regex pattern node.
     *
     * @param regexPattern The base regex pattern node from Langkit.
     * @return The regex pattern node for Truffle.
     */
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
        final List<BasePattern> tuplePatterns = new ArrayList<>();
        for (Liblkqllang.LkqlNode pattern : tuplePattern.fPatterns().children()) {
            tuplePatterns.add((BasePattern) pattern.accept(this));
        }

        return TuplePatternNodeGen.create(
            loc(tuplePattern),
            tuplePatterns.toArray(new BasePattern[0])
        );
    }

    /**
     * Visit a parented pattern node.
     *
     * @param parenPattern The parented pattern node from Langkit.
     * @return The parented pattern node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.ParenPattern parenPattern) {
        // Translate the parented pattern
        final BasePattern pattern = (BasePattern) parenPattern.fPattern().accept(this);

        // Return the new parented pattern node
        return new ParenPattern(loc(parenPattern), pattern);
    }

    /**
     * Visit an or pattern node.
     *
     * @param orPattern The or pattern node from Langkit.
     * @return The or pattern node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.OrPattern orPattern) {
        // Translate the or pattern fields
        final BasePattern left = (BasePattern) orPattern.fLeft().accept(this);
        final BasePattern right = (BasePattern) orPattern.fRight().accept(this);

        // Create the new or pattern node
        return new OrPattern(loc(orPattern), left, right);
    }

    /**
     * Visit a not pattern node.
     *
     * @param notPattern The not pattern node from Langkit
     * @return The not pattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NotPattern notPattern) {
        // Translate the not pattern fields
        final ValuePattern pattern = (ValuePattern) notPattern.fPattern().accept(this);

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

    /**
     * Visit a query node.
     *
     * @param query The query node from Langkit.
     * @return The query node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.Query query) {
        // Enter the query frame
        this.scriptFrames.enterFrame(query);

        // Translate the query fields
        boolean followGenerics = false;
        final Liblkqllang.Expr throughExprBase = query.fThroughExpr();
        Expr throughExpr = null;
        if (!throughExprBase.isNone()) {
            if (throughExprBase.getText().equals("follow_generics")) followGenerics = true;
            else throughExpr = (Expr) throughExprBase.accept(this);
        }

        final Liblkqllang.Expr fromExprBase = query.fFromExpr();
        final Expr fromExpr = fromExprBase.isNone() ? null : (Expr) fromExprBase.accept(this);

        final BasePattern pattern = (BasePattern) query.fPattern().accept(this);

        // Get the query kind
        final Query.Kind queryKind;
        final Liblkqllang.QueryKind queryKindBase = query.fQueryKind();
        if (queryKindBase instanceof Liblkqllang.QueryKindFirst) {
            queryKind = Query.Kind.FIRST;
        } else {
            queryKind = Query.Kind.ALL;
        }

        // Exit the query frame
        this.scriptFrames.exitFrame();

        // Return the new query node
        return new Query(loc(query), queryKind, followGenerics, throughExpr, fromExpr, pattern);
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

    /**
     * Visit a list literal node.
     *
     * @param listLiteral The list literal node from Langkit.
     * @return The list literal node for Truffle.
     */
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

    /**
     * Visit a list comprehension node.
     *
     * @param listComprehension The list comprehension node from Langkit.
     * @return The list comprehension node for Truffle.
     */
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
        this.scriptFrames.enterFrame(listComprehension);

        // Create the list comprehension associations
        final List<ComprehensionAssoc> assocList = new ArrayList<>();
        for (int i = 0; i < assocListBase.getChildrenCount(); i++) {
            final Liblkqllang.ListCompAssoc assocBase =
                (Liblkqllang.ListCompAssoc) assocListBase.getChild(i);
            final String name = assocBase.fBindingName().getText();
            int slot = this.scriptFrames.getParameter(name);

            assocList.add(new ComprehensionAssoc(loc(assocBase), name, slot, collections.get(i)));
        }

        // Translate the list comprehension fields
        final Expr expr = (Expr) listComprehension.fExpr().accept(this);
        final Liblkqllang.Expr guardBase = listComprehension.fGuard();
        final Expr guard = guardBase.isNone() ? null : (Expr) guardBase.accept(this);

        // Create the result
        final ListComprehension res = new ListComprehension(
            loc(listComprehension),
            this.scriptFrames.getFrameDescriptor(),
            this.scriptFrames.getClosureDescriptor(),
            new ComprehensionAssocList(
                loc(assocListBase),
                assocList.toArray(new ComprehensionAssoc[0])
            ),
            expr,
            guard
        );

        // Exit the frame
        this.scriptFrames.exitFrame();

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

    /**
     * Visit an object literal node.
     *
     * @param objectLiteral The object literal node from Langkit.
     * @return The object literal node for Truffle.
     */
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
                throw multipleSameNameKeys(objectLiteral, key);
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

    /**
     * Visit an at object literal node.
     *
     * @param atObjectLiteral The at object literal node from Langkit.
     * @return The equivalent object literal node for Truffle.
     */
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
                throw multipleSameNameKeys(atObjectLiteral, key);
            }
            keys.add(key);
            final Liblkqllang.Expr exprBase = assoc.fExpr();
            values[i] = exprBase.isNone()
                ? new ListLiteral(
                    loc(assoc.fName()),
                    new Expr[] { new ObjectLiteral(loc(assoc.fName()), new String[0], new Expr[0]) }
                )
                : AtObjectValueWrapperNodeGen.create(loc(exprBase), (Expr) exprBase.accept(this));
        }

        // Return the new object literal node because at object is juste syntactic sugar
        return new ObjectLiteral(loc(atObjectLiteral), keys.toArray(new String[0]), values);
    }

    // --- Block expressions

    /**
     * Visit a block body declaration node.
     *
     * @param blockBodyDecl The block body declaration node from Langkit.
     * @return The block body declaration node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.BlockBodyDecl blockBodyDecl) {
        // Translate the block body fields
        final Declaration declaration = (Declaration) blockBodyDecl.fDecl().accept(this);

        // Return the new block body declaration node
        return new BlockBodyDecl(loc(blockBodyDecl), declaration);
    }

    /**
     * Visit a block body expression node.
     *
     * @param blockBodyExpr The block body expression node from Langkit.
     * @return The block body expression node for Truffle.
     */
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

    /**
     * Visit a block expression node.
     *
     * @param blockExpr The block expression node from Langkit.
     * @return The block expression node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.BlockExpr blockExpr) {
        // Enter the block expression frame
        this.scriptFrames.enterFrame(blockExpr);

        // Create the declaration list
        final List<BlockBody> blockBody = new ArrayList<>();
        for (Liblkqllang.LkqlNode bodyPart : blockExpr.fBody().children()) {
            blockBody.add((BlockBody) bodyPart.accept(this));
        }

        // Get the expression of the block
        final Expr expr = (Expr) blockExpr.fExpr().accept(this);

        // Exit the block expression frame
        this.scriptFrames.exitFrame();

        // Return the result
        return new BlockExpr(loc(blockExpr), blockBody.toArray(new BlockBody[0]), expr);
    }

    // --- Function declarations

    /**
     * Visit a named function node.
     *
     * @param namedFunction The named function node from Langkit.
     * @return The named function node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.NamedFunction namedFunction) {
        return this.functionExprHelper(
                namedFunction,
                ((Liblkqllang.FunDecl) namedFunction.parent()).fName().getText()
            );
    }

    /**
     * Visit an anonymous function node.
     *
     * @param anonymousFunction The anonymous function node from Langkit.
     * @return The anonymous function node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.AnonymousFunction anonymousFunction) {
        return this.functionExprHelper(anonymousFunction, "<anonymous>");
    }

    /**
     * Helper function to visit named and anonymous function the same way.
     *
     * @param baseFunction The base function node from Langkit.
     * @return The function expression node for Truffle.
     */
    private FunExpr functionExprHelper(Liblkqllang.BaseFunction baseFunction, String name) {
        // Enter the function frame
        this.scriptFrames.enterFrame(baseFunction);

        var names = toStream(baseFunction.fParameters())
            .map(p -> p.fParamIdentifier().getText())
            .toArray(String[]::new);
        var defaultVals = toStream(baseFunction.fParameters())
            .map(p -> p.fDefaultExpr().isNone() ? null : (Expr) p.fDefaultExpr().accept(this))
            .toArray(Expr[]::new);
        var body = (Expr) baseFunction.fBodyExpr().accept(this);
        var docstring = baseFunction.pDoc();

        // Return the new function expression node
        var res = new FunExpr(
            loc(baseFunction),
            this.scriptFrames.getFrameDescriptor(),
            this.scriptFrames.getClosureDescriptor(),
            names,
            defaultVals,
            docstring.isNone() ? "" : parseStringLiteral(docstring),
            body,
            name
        );

        // Exit the function frame
        this.scriptFrames.exitFrame();

        // Return the result
        return res;
    }

    /**
     * Visit a function declaration node.
     *
     * @param funDecl The function declaration node from Langkit.
     * @return The function declaration node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.FunDecl funDecl) {
        // Get the current slot of the name
        final String name = funDecl.fName().getText();
        this.scriptFrames.declareBinding(name);
        final int slot = this.scriptFrames.getBinding(name);

        // Translate the declaration annotation
        final Liblkqllang.DeclAnnotation annotationBase = funDecl.fAnnotation();
        final Annotation annotation = annotationBase.isNone()
            ? null
            : (Annotation) annotationBase.accept(this);

        // Translate the function body
        FunExpr funExpr = (FunExpr) funDecl.fFunExpr().accept(this);

        // Create the new function declaration node
        final var functionDecl = new FunctionDeclaration(loc(funDecl), annotation, slot, funExpr);

        // If the function is annotated as a checker, create a checker exportation node and
        // return it
        if (annotation != null) {
            if (annotation.getName().equals(Constants.ANNOTATION_NODE_CHECK)) {
                return new CheckerExport(
                    loc(funDecl),
                    annotation,
                    CheckerExport.CheckerMode.NODE,
                    functionDecl
                );
            } else if (annotation.getName().equals(Constants.ANNOTATION_UNIT_CHECK)) {
                return new CheckerExport(
                    loc(funDecl),
                    annotation,
                    CheckerExport.CheckerMode.UNIT,
                    functionDecl
                );
            }
        }

        // Finally return the function declaration
        return functionDecl;
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

    /**
     * Visit a function call node.
     *
     * @param funCall The function call node from Langkit.
     * @return The function call node for Truffle.
     */
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
     * If the node is encountered while parsing a 'pass', it is instead instanciated
     * as a dynamic constructor call (creating a dynamically typed node).
     *
     * @param constructorCall The constructor call from Langkit.
     * @return The constructor call node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.ConstructorCall constructorCall) {
        if (inPass) {
            return new DynamicConstructorCall(
                loc(constructorCall),
                constructorCall.fName().getText(),
                (ArgList) constructorCall.fArguments().accept(this)
            );
        } else {
            return new ConstructorCall(
                loc(constructorCall),
                new Identifier(loc(constructorCall.fName()), constructorCall.fName().getText()),
                (ArgList) constructorCall.fArguments().accept(this)
            );
        }
    }

    // --- Selector declaration

    @Override
    public LKQLNode visit(Liblkqllang.SelectorArmList selectorArmList) {
        return null;
    }

    /**
     * Visit a selector declaration node.
     *
     * @param selectorDecl The selector declaration node from Langkit.
     * @return The selector declaration node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorDecl selectorDecl) {
        // Get the name, documentation and annotation for the selector declaration
        final String name = selectorDecl.fName().getText();
        final Liblkqllang.BaseStringLiteral documentationBase = selectorDecl.fDocNode();
        final String documentation = documentationBase.isNone()
            ? ""
            : parseStringLiteral(documentationBase);

        // Get the current slot to place the new selector in
        this.scriptFrames.declareBinding(name);
        final int slot = this.scriptFrames.getBinding(name);

        // Translate the declaration annotation
        final Liblkqllang.DeclAnnotation annotationBase = selectorDecl.fAnnotation();
        final Annotation annotation = annotationBase.isNone()
            ? null
            : (Annotation) annotationBase.accept(this);

        // Enter the selector frame
        this.scriptFrames.enterFrame(selectorDecl);

        // Get the "this" and "depth" bindings
        final int thisSlot = this.scriptFrames.getParameter(Constants.THIS_SYMBOL);

        var arms = toStream(selectorDecl.fArms())
            .map(a -> {
                // Enter the arm's frame
                this.scriptFrames.enterFrame(a);

                // Translate the arm's fields
                final BasePattern pattern = (BasePattern) a.fPattern().accept(this);
                final Expr expr = (Expr) a.fExpr().accept(this);

                // Exit the arm's frame
                this.scriptFrames.exitFrame();

                // Return the new match arm
                return new MatchArm(loc(a), pattern, expr);
            })
            .toArray(MatchArm[]::new);

        var match = new Match(
            loc(selectorDecl),
            new ReadParameter(loc(selectorDecl), thisSlot),
            arms
        );

        // Create the frame descriptor for the arms
        var frameDescriptor = this.scriptFrames.getFrameDescriptor();
        var closureDescriptor = this.scriptFrames.getClosureDescriptor();

        // Exit the selector frame
        this.scriptFrames.exitFrame();

        // Return the new selector declaration node
        return new SelectorDeclaration(
            loc(selectorDecl),
            annotation,
            frameDescriptor,
            closureDescriptor,
            name,
            documentation,
            slot,
            match
        );
    }

    /**
     * Visit a selector call node.
     *
     * @param selectorCall The selector call node from Langkit.
     * @return The selector call node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorCall selectorCall) {
        // Translate the selector call fields
        final SelectorCall.Quantifier quantifier;
        if (selectorCall.fQuantifier().getText().equals("any")) {
            quantifier = SelectorCall.Quantifier.ANY;
        } else {
            quantifier = SelectorCall.Quantifier.ALL;
        }

        final Liblkqllang.Identifier bindingBase = selectorCall.fBinding();
        final String binding = bindingBase.isNone() ? null : bindingBase.getText();

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

        // Get the slot for the binding
        final int bindingSlot;
        if (binding != null) {
            this.scriptFrames.declareBinding(binding);
            bindingSlot = this.scriptFrames.getBinding(binding);
        } else {
            bindingSlot = -1;
        }

        // Return the new node
        return new SelectorCall(loc(selectorCall), quantifier, bindingSlot, selectorExpr, args);
    }

    // --- Node patterns

    /**
     * Visit a node kind pattern node.
     *
     * @param nodeKindPattern The node kind pattern node from Langkit.
     * @return The node kind pattern node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.NodeKindPattern nodeKindPattern) {
        return new NodeKindPattern(loc(nodeKindPattern), nodeKindPattern.fKindName().getText());
    }

    /**
     * Visit a node pattern field node.
     *
     * @param nodePatternField The node pattern field node from Langkit.
     * @return The node pattern field node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.NodePatternField nodePatternField) {
        // Translate the node pattern detail fields
        final String name = nodePatternField.fIdentifier().getText();
        final BasePattern expected = (BasePattern) nodePatternField.fExpectedValue().accept(this);

        nodePatternField.fPatternDetailDelimiter().accept(this);

        // Return the new node pattern field detail
        return NodePatternFieldNodeGen.create(loc(nodePatternField), name, expected);
    }

    /**
     * Visit a node pattern property node.
     *
     * @param nodePatternProperty The base node pattern property node from Langkit.
     * @return The node pattern property node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.NodePatternProperty nodePatternProperty) {
        // Translate the node pattern detail fields
        final String propertyName = nodePatternProperty.fCall().fName().getText();
        final ArgList argList = (ArgList) nodePatternProperty.fCall().fArguments().accept(this);
        final BasePattern expected = (BasePattern) nodePatternProperty
            .fExpectedValue()
            .accept(this);

        nodePatternProperty.fPatternDetailDelimiter().accept(this);

        // Return the new node pattern property detail
        return NodePatternPropertyNodeGen.create(
            loc(nodePatternProperty),
            propertyName,
            Arrays.stream(argList.getArgs()).map(Arg::getArgExpr).toArray(Expr[]::new),
            expected
        );
    }

    /**
     * Visit a node pattern selector node.
     *
     * @param nodePatternSelector The node pattern selector node from Langkit.
     * @return The node pattern selector node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.NodePatternSelector nodePatternSelector) {
        // Translate the node pattern selector
        final SelectorCall selectorCall = (SelectorCall) nodePatternSelector.fCall().accept(this);

        // Enter the node pattern selector frame
        this.scriptFrames.enterFrame(nodePatternSelector.fPattern());

        // Translate the node pattern selector pattern
        final BasePattern pattern = (BasePattern) nodePatternSelector.fPattern().accept(this);

        // Exit the node pattern selector frame
        this.scriptFrames.exitFrame();

        nodePatternSelector.fPatternDetailDelimiter().accept(this);

        // Return the new selector node pattern detail
        return new NodePatternSelector(loc(nodePatternSelector), selectorCall, pattern);
    }

    @Override
    public LKQLNode visit(Liblkqllang.NodePatternDetailList nodePatternDetailList) {
        return null;
    }

    /**
     * Visit an extended node kind pattern node.
     *
     * @param extendedNodePattern The extended node kind node from Langkit.
     * @return The extended node kind node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.ExtendedNodePattern extendedNodePattern) {
        // Translate the extended node pattern fields
        final ValuePattern nodePattern = (ValuePattern) extendedNodePattern
            .fNodePattern()
            .accept(this);

        // Get the pattern details
        final List<NodePatternDetail> details = new ArrayList<>();
        for (Liblkqllang.LkqlNode node : extendedNodePattern.fDetails().children()) {
            details.add((NodePatternDetail) node.accept(this));
        }

        // Return the new extended node pattern node
        return new ExtendedNodePattern(
            loc(extendedNodePattern),
            nodePattern,
            details.toArray(new NodePatternDetail[0])
        );
    }

    // --- Pattern matching

    /**
     * Visit a match arm node.
     *
     * @param matchArm The match arm node from Langkit.
     * @return The match arm node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.MatchArm matchArm) {
        // Enter the match arm frame
        this.scriptFrames.enterFrame(matchArm);

        // Translate the match arm fields
        final BasePattern pattern = (BasePattern) matchArm.fPattern().accept(this);
        final Expr expr = (Expr) matchArm.fExpr().accept(this);

        // Exit the match arm frame
        this.scriptFrames.exitFrame();

        // Return the new match arm
        return new MatchArm(loc(matchArm), pattern, expr);
    }

    @Override
    public LKQLNode visit(Liblkqllang.MatchArmList matchArmList) {
        return null;
    }

    /**
     * Visit a match node.
     *
     * @param match The match node from Langkit.
     * @return The match node for Truffle.
     */
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

    /**
     * Visit a import node.
     *
     * @param anImport The import node from Langkit.
     * @return The import node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.Import anImport) {
        // Get the name of the import
        final String name = anImport.fName().getText();

        // Get the slot to place the import in
        this.scriptFrames.declareBinding(name);
        final int slot = this.scriptFrames.getBinding(name);

        // Return the import node
        return new Import(loc(anImport), name, slot);
    }

    // --- Tuples

    /**
     * Visit a tuple literal node.
     *
     * @param tuple The tuple literal node from Langkit.
     * @return The tuple literal node for Truffle.
     */
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
        if (addCollector.size() != 1) throw translationError(
            passDecl,
            "pass declaration should contain exactly one `add' block"
        );
        if (delCollector.size() != 1) throw translationError(
            passDecl,
            "pass declaration should contain exactly one `del' block"
        );
        if (rewriteCollector.size() != 1) throw translationError(
            passDecl,
            "pass declaration should contain exactly one `rewrite' block"
        );

        // Register pass as fake function
        final String name = passDecl.fPassName().getText();
        scriptFrames.declareBinding(name);
        final int slot = scriptFrames.getBinding(name);
        final Liblkqllang.Identifier previousName = passDecl.fPreviousPassName();
        final Optional<Integer> previousSlot = previousName.isNone()
            ? Optional.empty()
            : Optional.of(scriptFrames.getBinding(previousName.getText()));

        // Enter pass lexical environement
        inPass = true;
        scriptFrames.enterFrame(passDecl);

        final int nbFakeArgs = Constants.PASS_FAKE_ARGS.length;
        final var parameterDeclarations = new ParameterDeclaration[nbFakeArgs];
        final var readArguments = new ReadArgument[nbFakeArgs];
        for (int i = 0; i < nbFakeArgs; i++) {
            final String argName = Constants.PASS_FAKE_ARGS[i];
            parameterDeclarations[i] = new ParameterDeclaration(
                source.createUnavailableSection(),
                argName,
                null
            );
            readArguments[i] = new ReadArgument(
                source.createUnavailableSection(),
                scriptFrames.getParameter(argName)
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
            readArguments[0]
        );

        final var docstring = passDecl.pDoc();

        final var funExpr = new FunExpr(
            location,
            scriptFrames.getFrameDescriptor(),
            scriptFrames.getClosureDescriptor(),
            parameterDeclarations,
            docstring.isNone() ? "" : parseStringLiteral(docstring),
            passExpr
        );

        // Cleanup
        inPass = false;
        this.scriptFrames.exitFrame();

        final var functionDecl = new FunctionDeclaration(location, null, name, slot, funExpr);

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
                    throw LKQLRuntimeException.shouldNotHappen(
                        "found node of type " + translated.getClass().getCanonicalName()
                    );
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
                    throw LKQLRuntimeException.shouldNotHappen(
                        "found node of type " + clause.getClass().getCanonicalName()
                    );
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
        final int slot = scriptFrames.getBinding(name);
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
