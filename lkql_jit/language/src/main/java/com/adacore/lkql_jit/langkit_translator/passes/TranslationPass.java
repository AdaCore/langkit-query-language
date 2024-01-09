/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.langkit_translator.passes;

import com.adacore.liblkqllang.Liblkqllang;
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
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorArm;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorDeclaration;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorExpr;
import com.adacore.lkql_jit.nodes.expressions.*;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyDecl;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyExpr;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockExpr;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.dot.SafeDotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ComprehensionAssoc;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ComprehensionAssocList;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ListComprehension;
import com.adacore.lkql_jit.nodes.expressions.literals.*;
import com.adacore.lkql_jit.nodes.expressions.match.Match;
import com.adacore.lkql_jit.nodes.expressions.match.MatchArm;
import com.adacore.lkql_jit.nodes.expressions.operators.*;
import com.adacore.lkql_jit.nodes.expressions.value_read.*;
import com.adacore.lkql_jit.nodes.patterns.*;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.*;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.source.Source;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
    private SourceLocation loc(final Liblkqllang.LkqlNode node) {
        return new SourceLocation(this.source, node.getSourceLocationRange());
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
            for (Liblkqllang.LkqlNode subBlock :
                    ((Liblkqllang.BlockStringLiteral) stringLiteral).fDocs().children()) {
                var str = StringUtils.translateEscapes(subBlock.getText().substring(2));

                if (str.length() > 0) {
                    // First character should be a whitespace, as specified in
                    // the user manual.
                    if (str.charAt(0) != ' ') {
                        throw LKQLRuntimeException.fromMessage(
                                "Invalid blockstring: first character should be whitespace");
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
                doc);
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
        final SourceLocation location = loc(identifier);

        // First look for the symbol in the frame local bindings
        if (this.scriptFrames.isBinding(symbol) && this.scriptFrames.isBindingDeclared(symbol)) {
            return new ReadLocal(location, this.scriptFrames.getBinding(symbol));
        }

        // In a second time look in the parameters of the frame
        else if (this.scriptFrames.isParameter(symbol)) {
            return new ReadArgument(location, this.scriptFrames.getParameter(symbol));
        }

        // Then look in the closure for the symbol
        else if (this.scriptFrames.isClosure(symbol)) {
            final int slot = this.scriptFrames.getClosure(symbol);
            if (this.scriptFrames.isClosureDeclared(symbol)) {
                return new ReadClosure(location, slot);
            } else {
                return new ReadClosureUnsafe(location, slot, symbol);
            }
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
        throw LKQLRuntimeException.unknownSymbol(symbol, new DummyLocation(loc(identifier)));
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
                    loc(integerLiteral), new BigInteger(integerLiteral.getText()));
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
    public LKQLNode visit(Liblkqllang.SubBlockLiteralList subBlockLiteralList) {
        return null;
    }

    // --- If then else node

    /**
     * Visit an "if then else" node.
     *
     * @param ifThenElse The "if then else" node from Langkit.
     * @return The "if then else" node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.IfThenElse ifThenElse) {
        // Translate the if then else fields
        final Expr condition = (Expr) ifThenElse.fCondition().accept(this);
        final Expr consequence = (Expr) ifThenElse.fThenExpr().accept(this);
        final Expr alternative = (Expr) ifThenElse.fElseExpr().accept(this);

        // Return the if then else node
        return new IfThenElse(loc(ifThenElse), condition, consequence, alternative);
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
                if (seenNames.contains(namedArg.getArgName().getName())) {
                    throw LKQLRuntimeException.multipleSameNameArgument(curArg);
                }
                seenNames.add(namedArg.getArgName().getName());
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
        // Translate the parameter declaration fields
        final String name = parameterDecl.fParamIdentifier().getText();
        final Liblkqllang.Expr defaultExpr = parameterDecl.fDefaultExpr();
        final Expr defaultValue = defaultExpr.isNone() ? null : (Expr) defaultExpr.accept(this);

        // Return the new parameter declaration
        return new ParameterDeclaration(
                new SourceLocation(this.source, parameterDecl.getSourceLocationRange()),
                name,
                defaultValue);
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

        final DummyLocation leftLocation = new DummyLocation(left.getLocation());
        final DummyLocation rightLocation = new DummyLocation(right.getLocation());
        final SourceLocation location = loc(binOp);

        // Create the binary operator by switching on the operator type
        return switch (binOp.fOp().getKindName()) {
            case Liblkqllang.OpPlus.kindName -> BinPlusNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpMinus.kindName -> BinMinusNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpMul.kindName -> BinMulNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpDiv.kindName -> BinDivNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpAnd.kindName -> new BinAnd(location, left, right);
            case Liblkqllang.OpOr.kindName -> new BinOr(location, left, right);
            case Liblkqllang.OpEq.kindName -> BinEqNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpNeq.kindName -> BinNeqNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpConcat.kindName -> BinConcatNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpLt.kindName -> BinLtNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpLeq.kindName -> BinLeqNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpGt.kindName -> BinGtNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpGeq.kindName -> BinGeqNodeGen.create(
                    location, leftLocation, rightLocation, left, right);
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

        final DummyLocation argLocation = new DummyLocation(arg.getLocation());
        final SourceLocation location =
                new SourceLocation(this.source, unOp.getSourceLocationRange());

        // Create the unary operator by switching on the operator type
        return switch (unOp.fOp().getKindName()) {
            case Liblkqllang.OpNot.kindName -> UnNotNodeGen.create(location, argLocation, arg);
            case Liblkqllang.OpPlus.kindName -> UnPlusNodeGen.create(location, argLocation, arg);
            case Liblkqllang.OpMinus.kindName -> UnMinusNodeGen.create(location, argLocation, arg);
            default -> null;
        };
    }

    // --- Operators

    @Override
    public LKQLNode visit(Liblkqllang.OpPlus opPlus) {
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

    // --- Unpack node

    /**
     * Visit an unpack node.
     *
     * @param unpack The unpack node from Langkit.
     * @return The unpack node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.Unpack unpack) {
        // Translate the unpack field
        final Expr collectionExpr = (Expr) unpack.fCollectionExpr().accept(this);

        // Return the new unpack node
        return new Unpack(loc(unpack), collectionExpr);
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
        return DotAccessNodeGen.create(
                loc(dotAccess),
                new Identifier(loc(memberIdentifier), memberIdentifier.getText()),
                receiver);
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
        return SafeDotAccessNodeGen.create(
                loc(safeAccess),
                new Identifier(loc(memberIdentifier), memberIdentifier.getText()),
                receiver);
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
        return InClauseNodeGen.create(
                loc(inClause),
                new DummyLocation(elem.getLocation()),
                new DummyLocation(indexable.getLocation()),
                elem,
                indexable);
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
        final UnfilteredPattern pattern =
                (UnfilteredPattern) filteredPattern.fPattern().accept(this);
        final Expr predicate = (Expr) filteredPattern.fPredicate().accept(this);

        // Create the new filtered pattern node
        return new FilteredPattern(loc(filteredPattern), pattern, predicate);
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

        // Visit the associated value pattern
        final ValuePattern pattern = (ValuePattern) bindingPattern.fValuePattern().accept(this);

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
        return IsClauseNodeGen.create(
                loc(isClause), new DummyLocation(nodeExpr.getLocation()), pattern, nodeExpr);
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
        final ListComprehension res =
                new ListComprehension(
                        loc(listComprehension),
                        this.scriptFrames.getFrameDescriptor(),
                        this.scriptFrames.getClosureDescriptor(),
                        new ComprehensionAssocList(
                                new SourceLocation(
                                        this.source, assocListBase.getSourceLocationRange()),
                                assocList.toArray(new ComprehensionAssoc[0])),
                        expr,
                        guard);

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
        final String[] keys = new String[assocNumber];
        final Expr[] values = new Expr[assocNumber];

        // Iterate on the object associations and get keys and values
        for (int i = 0; i < assocNumber; i++) {
            final Liblkqllang.ObjectAssoc assoc = (Liblkqllang.ObjectAssoc) assocList.getChild(i);
            keys[i] = assoc.fName().getText();
            values[i] = (Expr) assoc.fExpr().accept(this);
        }

        // Return the new object literal
        return new ObjectLiteral(loc(objectLiteral), keys, values);
    }

    @Override
    public LKQLNode visit(Liblkqllang.AtObjectAssoc atObjectAssoc) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.AtObjectAssocList atObjectAssocList) {
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
        final String[] keys = new String[assocNumber];
        final Expr[] values = new Expr[assocNumber];

        // Iterate on object associations and translate them
        for (int i = 0; i < assocNumber; i++) {
            final Liblkqllang.AtObjectAssoc assoc =
                    (Liblkqllang.AtObjectAssoc) assocList.getChild(i);
            keys[i] = assoc.fName().getText();
            final Liblkqllang.Expr exprBase = assoc.fExpr();
            values[i] =
                    exprBase.isNone()
                            ? new ListLiteral(loc(assoc.fName()), new Expr[0])
                            : (Expr) exprBase.accept(this);
        }

        // Return the new object literal node because at object is juste syntactic sugar
        return new ObjectLiteral(loc(atObjectLiteral), keys, values);
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
        return this.functionExprHelper(namedFunction);
    }

    /**
     * Visit an anonymous function node.
     *
     * @param anonymousFunction The anonymous function node from Langkit.
     * @return The anonymous function node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.AnonymousFunction anonymousFunction) {
        return this.functionExprHelper(anonymousFunction);
    }

    /**
     * Helper function to visit named and anonymous function the same way.
     *
     * @param baseFunction The base function node from Langkit.
     * @return The function expression node for Truffle.
     */
    private FunExpr functionExprHelper(Liblkqllang.BaseFunction baseFunction) {
        // Enter the function frame
        this.scriptFrames.enterFrame(baseFunction);

        // Translate the function fields
        final List<ParameterDeclaration> parameters = new ArrayList<>();
        for (Liblkqllang.LkqlNode param : baseFunction.fParameters().children()) {
            parameters.add((ParameterDeclaration) param.accept(this));
        }
        final Expr body = (Expr) baseFunction.fBodyExpr().accept(this);

        final var docstring = baseFunction.pDoc();

        // Return the new function expression node
        final FunExpr res =
                new FunExpr(
                        loc(baseFunction),
                        this.scriptFrames.getFrameDescriptor(),
                        this.scriptFrames.getClosureDescriptor(),
                        parameters.toArray(new ParameterDeclaration[0]),
                        docstring.isNone() ? "" : parseStringLiteral(docstring),
                        body);

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
        // Translate the declaration fields
        final Liblkqllang.DeclAnnotation annotationBase = funDecl.fAnnotation();
        final Annotation annotation =
                annotationBase.isNone() ? null : (Annotation) annotationBase.accept(this);
        final String name = funDecl.fName().getText();

        // Get the current slot of the name
        this.scriptFrames.declareBinding(name);
        final int slot = this.scriptFrames.getBinding(name);

        // Translate the function body
        FunExpr funExpr = (FunExpr) funDecl.fFunExpr().accept(this);

        // Return the new function declaration node
        return new FunctionDeclaration(loc(funDecl), annotation, name, slot, funExpr);
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
                loc(funCall), isSafe, new DummyLocation(callee.getLocation()), arguments, callee);
    }

    // --- Selector declaration

    @Override
    public LKQLNode visit(Liblkqllang.SelectorExprModeDefault selectorExprModeDefault) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorExprModeRec selectorExprModeRec) {
        return null;
    }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorExprModeSkip selectorExprModeSkip) {
        return null;
    }

    /**
     * Visit a selector expression node.
     *
     * @param selectorExpr The selector expression node from Langkit.
     * @return The selector expression node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorExpr selectorExpr) {
        // Translate the selector expression fields
        final Expr expr = (Expr) selectorExpr.fExpr().accept(this);

        // Get the expression mode
        // TODO: Create specialized node for each mode
        SelectorExpr.Mode mode = SelectorExpr.Mode.DEFAULT;
        final Liblkqllang.SelectorExprMode modeNode = selectorExpr.fMode();
        if (modeNode instanceof Liblkqllang.SelectorExprModeRec) {
            mode = SelectorExpr.Mode.REC;
        } else if (modeNode instanceof Liblkqllang.SelectorExprModeSkip) {
            mode = SelectorExpr.Mode.SKIP;
        }

        // Create the new selector expression
        return new SelectorExpr(loc(selectorExpr), mode, expr);
    }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorExprList selectorExprList) {
        return null;
    }

    /**
     * Visit a selector arm node.
     *
     * @param selectorArm The selector arm node from Langkit.
     * @return The selector arm node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorArm selectorArm) {
        // Enter the arm frame
        this.scriptFrames.enterFrame(selectorArm);

        // Translate the selector arm fields
        // TODO: Question on why many expressions by arm
        final BasePattern pattern = (BasePattern) selectorArm.fPattern().accept(this);
        final SelectorExpr expr = (SelectorExpr) selectorArm.fExprsList().getChild(0).accept(this);

        // Exit the arm frame
        this.scriptFrames.exitFrame();

        // Return the selector arm node
        return new SelectorArm(loc(selectorArm), pattern, expr);
    }

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
        final String documentation =
                documentationBase.isNone() ? "" : parseStringLiteral(documentationBase);
        final Liblkqllang.DeclAnnotation annotationBase = selectorDecl.fAnnotation();
        final Annotation annotation =
                annotationBase.isNone() ? null : (Annotation) annotationBase.accept(this);

        // Get the current slot to place the new selector in
        this.scriptFrames.declareBinding(name);
        final int slot = this.scriptFrames.getBinding(name);

        // Enter the selector frame
        this.scriptFrames.enterFrame(selectorDecl);

        // Get the "this" and "depth" bindings
        this.scriptFrames.declareBinding(Constants.THIS_SYMBOL);
        this.scriptFrames.declareBinding(Constants.DEPTH_SYMBOL);
        final int thisSlot = this.scriptFrames.getBinding(Constants.THIS_SYMBOL);
        final int depthSlot = this.scriptFrames.getBinding(Constants.DEPTH_SYMBOL);

        // Get the selector arms
        final List<SelectorArm> arms = new ArrayList<>();
        for (Liblkqllang.LkqlNode node : selectorDecl.fArms().children()) {
            arms.add((SelectorArm) node.accept(this));
        }

        // Create the frame descriptor for the arms
        FrameDescriptor frameDescriptor = this.scriptFrames.getFrameDescriptor();
        ClosureDescriptor closureDescriptor = this.scriptFrames.getClosureDescriptor();

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
                thisSlot,
                depthSlot,
                arms.toArray(new SelectorArm[0]));
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
     * Visit a detail expression node.
     *
     * @param detailExpr The detail expression node from Langkit.
     * @return The detail expression node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.DetailExpr detailExpr) {
        // Translate the detail expression fields
        final Expr expr = (Expr) detailExpr.fExprValue().accept(this);

        // Return the new detail expression node
        return DetailExprNodeGen.create(loc(detailExpr), expr);
    }

    /**
     * Visit a detail pattern node.
     *
     * @param detailPattern The detail pattern node from Langkit
     * @return The detail pattern node for Truffle.
     */
    @Override
    public LKQLNode visit(Liblkqllang.DetailPattern detailPattern) {
        // Translate the detail pattern fields
        final BasePattern pattern = (BasePattern) detailPattern.fPatternValue().accept(this);

        // Return the new detail pattern node
        return new DetailPattern(loc(detailPattern), pattern);
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
        final DetailValue expected = (DetailValue) nodePatternField.fExpectedValue().accept(this);

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
        final DetailValue expected =
                (DetailValue) nodePatternProperty.fExpectedValue().accept(this);

        // Return the new node pattern property detail
        return NodePatternPropertyNodeGen.create(
                loc(nodePatternProperty), propertyName, argList, expected);
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
        final ValuePattern nodePattern =
                (ValuePattern) extendedNodePattern.fNodePattern().accept(this);

        // Get the pattern details
        final List<NodePatternDetail> details = new ArrayList<>();
        for (Liblkqllang.LkqlNode node : extendedNodePattern.fDetails().children()) {
            details.add((NodePatternDetail) node.accept(this));
        }

        // Return the new extended node pattern node
        return new ExtendedNodePattern(
                loc(extendedNodePattern), nodePattern, details.toArray(new NodePatternDetail[0]));
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
}
