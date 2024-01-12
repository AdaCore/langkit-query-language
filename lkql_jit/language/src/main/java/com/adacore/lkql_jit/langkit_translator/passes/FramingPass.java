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
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltInsHolder;
import com.adacore.lkql_jit.built_ins.selectors.BuiltInSelector;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.TranslatorException;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFramesBuilder;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.source.Source;

/** This class represents the framing pass for the Langkit AST translation process. */
public final class FramingPass implements Liblkqllang.BasicVisitor<Void> {

    // ----- Attributes -----

    /** Truffle source of the AST. */
    private final Source source;

    /** Script frames builder object. */
    private final ScriptFramesBuilder scriptFramesBuilder;

    // ----- Attributes -----

    /**
     * Create a new framing pass for the given source.
     *
     * @param source The Truffle source.
     */
    public FramingPass(final Source source) {
        this.source = source;
        this.scriptFramesBuilder = new ScriptFramesBuilder();
    }

    // ----- Getters -----

    public ScriptFramesBuilder getScriptFramesBuilder() {
        return this.scriptFramesBuilder;
    }

    // ----- Instance methods -----

    /**
     * Util method to create a source location for the given Langkit node.
     *
     * @param node The node to create a source location for.
     * @return A source location for the given node.
     */
    private DummyLocation loc(final Liblkqllang.LkqlNode node) {
        return new DummyLocation(new SourceLocation(this.source, node.getSourceLocationRange()));
    }

    /**
     * Check that the given symbol doesn't exist in the current bindings. Throw an exception if it
     * does.
     *
     * @param symbol The symbol to check.
     * @param node The node where the symbol was introduced.
     * @throws LKQLRuntimeException If the symbol is already in the current bindings.
     */
    private void checkDuplicateBindings(final String symbol, final Liblkqllang.LkqlNode node)
            throws LKQLRuntimeException {
        if (this.scriptFramesBuilder.bindingExists(symbol)) {
            throw LKQLRuntimeException.existingSymbol(symbol, loc(node));
        }
    }

    /**
     * Check that the given symbol doesn't exist in the current parameters. Throw an exception if it
     * does.
     *
     * @param symbol The symbol to check.
     * @param node The node where the symbol was introduced.
     * @throws LKQLRuntimeException If the symbol is already in the current parameters.
     */
    private void checkDuplicateParameters(final String symbol, final Liblkqllang.LkqlNode node)
            throws LKQLRuntimeException {
        if (this.scriptFramesBuilder.parameterExists(symbol)) {
            throw LKQLRuntimeException.existingParameter(symbol, loc(node));
        }
    }

    /**
     * Traverse the children of the given node.
     *
     * @param node The node to traverse the children of.
     */
    private void traverseChildren(final Liblkqllang.LkqlNode node) {
        int childrenCount = node.getChildrenCount();
        for (int i = 0; i < childrenCount; i++) {
            Liblkqllang.LkqlNode child = node.getChild(i);
            if (!child.isNone()) {
                child.accept(this);
            }
        }
    }

    // ----- Visiting methods -----

    // --- Top level nodes

    /**
     * Visit a raw LKQL node. Always raise an exception
     *
     * @param lkqlNode The raw LKQL node.
     */
    @Override
    public Void visit(Liblkqllang.LkqlNode lkqlNode) {
        throw new TranslatorException("Cannot visit a raw LKQL node during the framing pass");
    }

    /**
     * Create the frame description for a top level list.
     *
     * @param topLevelList The Langkit top level list node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.TopLevelList topLevelList) {
        this.scriptFramesBuilder.openFrame(topLevelList);

        // Add the built-in functions
        for (BuiltInFunctionValue function : BuiltInsHolder.get().builtInFunctions) {
            this.scriptFramesBuilder.addBuiltIn(function.getName());
        }

        // Add the built-in selectors
        for (BuiltInSelector selector : BuiltInsHolder.get().builtInSelectors) {
            this.scriptFramesBuilder.addBuiltIn(selector.getName());
        }

        this.traverseChildren(topLevelList);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    // --- Symbol introducer nodes

    /**
     * Create the frame description for a value declaration.
     *
     * @param valDecl The Langkit value declaration node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.ValDecl valDecl) {
        final String symbol = valDecl.fIdentifier().getText();
        checkDuplicateBindings(symbol, valDecl.fIdentifier());
        this.scriptFramesBuilder.addBinding(symbol);
        valDecl.fValue().accept(this);
        return null;
    }

    /**
     * Create the frame description for a function declaration.
     *
     * @param funDecl The Langkit function declaration node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.FunDecl funDecl) {
        final String symbol = funDecl.fName().getText();
        checkDuplicateBindings(symbol, funDecl.fName());
        this.scriptFramesBuilder.addBinding(symbol);
        funDecl.fFunExpr().accept(this);
        return null;
    }

    /**
     * Create the frame description for a selector arm.
     *
     * @param selectorArm The Langkit selector arm node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.SelectorArm selectorArm) {
        this.scriptFramesBuilder.openVirtualFrame(selectorArm);
        selectorArm.fPattern().accept(this);
        selectorArm.fExprsList().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for a selector declaration.
     *
     * @param selectorDecl The Langkit function selector node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.SelectorDecl selectorDecl) {
        // Add the symbol to the current bindings
        final String symbol = selectorDecl.fName().getText();
        checkDuplicateBindings(symbol, selectorDecl.fName());
        this.scriptFramesBuilder.addBinding(symbol);

        // Open a new frame, add implicit symbols and visit arms
        this.scriptFramesBuilder.openFrame(selectorDecl);
        this.scriptFramesBuilder.addBinding(Constants.THIS_SYMBOL);
        this.scriptFramesBuilder.addBinding(Constants.DEPTH_SYMBOL);
        selectorDecl.fArms().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for an import.
     *
     * @param anImport The Langkit import node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.Import anImport) {
        final String symbol = anImport.fName().getText();
        checkDuplicateBindings(symbol, anImport.fName());
        this.scriptFramesBuilder.addBinding(symbol);
        return null;
    }

    /**
     * Create the frame description for a parameter declaration.
     *
     * @param parameterDecl The Langkit parameter declaration node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.ParameterDecl parameterDecl) {
        final Liblkqllang.Expr defaultExpr = parameterDecl.fDefaultExpr();
        if (!defaultExpr.isNone()) {
            defaultExpr.accept(this);
        }
        final String symbol = parameterDecl.fParamIdentifier().getText();
        checkDuplicateParameters(symbol, parameterDecl);
        this.scriptFramesBuilder.addParameter(symbol);
        return null;
    }

    /**
     * Create the frame description for a selector call.
     *
     * @param selectorCall The Langkit selector call node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.SelectorCall selectorCall) {
        final Liblkqllang.Identifier binding = selectorCall.fBinding();
        if (!binding.isNone()) {
            final String symbol = binding.getText();
            checkDuplicateBindings(symbol, binding);
            this.scriptFramesBuilder.addBinding(symbol);
        }
        selectorCall.fSelectorCall().accept(this);
        return null;
    }

    /**
     * Create the frame description for a binding pattern.
     *
     * @param bindingPattern The Langkit binding pattern node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.BindingPattern bindingPattern) {
        final String symbol = bindingPattern.fBinding().getText();
        // TODO: Enable the duplicate binding detection here
        // checkDuplicateBindings(symbol, bindingPattern.fBinding());
        this.scriptFramesBuilder.addBinding(symbol);
        bindingPattern.fValuePattern().accept(this);
        return null;
    }

    // --- Frame opening nodes

    /**
     * Create the frame description for an anonymous function.
     *
     * @param anonymousFunction The Langkit anonymous function node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.AnonymousFunction anonymousFunction) {
        this.scriptFramesBuilder.openFrame(anonymousFunction);
        anonymousFunction.fParameters().accept(this);
        anonymousFunction.fBodyExpr().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for a named function.
     *
     * @param namedFunction The Langkit named function node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.NamedFunction namedFunction) {
        this.scriptFramesBuilder.openFrame(namedFunction);
        namedFunction.fParameters().accept(this);
        namedFunction.fBodyExpr().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for a list comprehension.
     *
     * @param listComprehension The Langkit list comprehension node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.ListComprehension listComprehension) {
        // Get the list comprehension generator list
        final Liblkqllang.ListCompAssocList generators = listComprehension.fGenerators();
        final int generatorsCount = generators.getChildrenCount();

        // Visit all generator expressions
        for (int i = 0; i < generatorsCount; i++) {
            final Liblkqllang.ListCompAssoc assoc =
                    (Liblkqllang.ListCompAssoc) generators.getChild(i);
            assoc.fCollExpr().accept(this);
        }

        // Open the frame and visit the list comprehension expressions
        this.scriptFramesBuilder.openFrame(listComprehension);
        for (int i = 0; i < generatorsCount; i++) {
            final Liblkqllang.ListCompAssoc assoc =
                    (Liblkqllang.ListCompAssoc) generators.getChild(i);
            final String symbol = assoc.fBindingName().getText();
            checkDuplicateParameters(symbol, assoc.fBindingName());
            this.scriptFramesBuilder.addParameter(symbol);
        }
        listComprehension.fExpr().accept(this);
        final Liblkqllang.Expr guard = listComprehension.fGuard();
        if (!guard.isNone()) {
            guard.accept(this);
        }
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for a block expression.
     *
     * @param blockExpr The Langkit block expression node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.BlockExpr blockExpr) {
        this.scriptFramesBuilder.openVirtualFrame(blockExpr);
        blockExpr.fBody().accept(this);
        blockExpr.fExpr().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for a query.
     *
     * @param query The Langkit query node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.Query query) {
        this.scriptFramesBuilder.openVirtualFrame(query);
        final Liblkqllang.Expr fromExpr = query.fFromExpr();
        if (!fromExpr.isNone()) {
            fromExpr.accept(this);
        }
        final Liblkqllang.Expr throughExpr = query.fThroughExpr();
        if (!throughExpr.isNone()) {
            throughExpr.accept(this);
        }
        query.fPattern().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for a match arm.
     *
     * @param matchArm The Langkit match arm node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.MatchArm matchArm) {
        this.scriptFramesBuilder.openVirtualFrame(matchArm);
        matchArm.fPattern().accept(this);
        matchArm.fExpr().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for an is clause.
     *
     * @param isClause The Langkit is clause node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.IsClause isClause) {
        isClause.fNodeExpr().accept(this);
        this.scriptFramesBuilder.openVirtualFrame(isClause);
        isClause.fPattern().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    /**
     * Create the frame description for a node pattern selector.
     *
     * @param nodePatternSelector The Langkit node pattern selector node.
     * @return Nothing.
     */
    @Override
    public Void visit(Liblkqllang.NodePatternSelector nodePatternSelector) {
        nodePatternSelector.fCall().accept(this);
        this.scriptFramesBuilder.openVirtualFrame(nodePatternSelector.fPattern());
        nodePatternSelector.fPattern().accept(this);
        this.scriptFramesBuilder.closeFrame();
        return null;
    }

    // --- Non frame-changing nodes

    @Override
    public Void visit(Liblkqllang.DeclAnnotation declAnnotation) {
        traverseChildren(declAnnotation);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ExprList exprList) {
        traverseChildren(exprList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BoolLiteralTrue boolLiteralTrue) {
        traverseChildren(boolLiteralTrue);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BoolLiteralFalse boolLiteralFalse) {
        traverseChildren(boolLiteralFalse);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.Identifier identifier) {
        traverseChildren(identifier);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.IntegerLiteral integerLiteral) {
        traverseChildren(integerLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.StringLiteral stringLiteral) {
        traverseChildren(stringLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BlockStringLiteral blockStringLiteral) {
        traverseChildren(blockStringLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.UnitLiteral unitLiteral) {
        traverseChildren(unitLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.NullLiteral nullLiteral) {
        traverseChildren(nullLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SubBlockLiteral subBlockLiteral) {
        traverseChildren(subBlockLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SubBlockLiteralList subBlockLiteralList) {
        traverseChildren(subBlockLiteralList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.IfThenElse ifThenElse) {
        traverseChildren(ifThenElse);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.Unwrap unwrap) {
        traverseChildren(unwrap);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ArgList argList) {
        traverseChildren(argList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ExprArg exprArg) {
        traverseChildren(exprArg);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.NamedArg namedArg) {
        traverseChildren(namedArg);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SynthNamedArg synthNamedArg) {
        traverseChildren(synthNamedArg);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ParameterDeclList parameterDeclList) {
        traverseChildren(parameterDeclList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpAnd opAnd) {
        traverseChildren(opAnd);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpConcat opConcat) {
        traverseChildren(opConcat);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpDiv opDiv) {
        traverseChildren(opDiv);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpEq opEq) {
        traverseChildren(opEq);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpGeq opGeq) {
        traverseChildren(opGeq);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpGt opGt) {
        traverseChildren(opGt);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpLeq opLeq) {
        traverseChildren(opLeq);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpLt opLt) {
        traverseChildren(opLt);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpMinus opMinus) {
        traverseChildren(opMinus);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpMul opMul) {
        traverseChildren(opMul);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpNeq opNeq) {
        traverseChildren(opNeq);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpNot opNot) {
        traverseChildren(opNot);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpOr opOr) {
        traverseChildren(opOr);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OpPlus opPlus) {
        traverseChildren(opPlus);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BinOp binOp) {
        traverseChildren(binOp);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.UnOp unOp) {
        traverseChildren(unOp);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.RelBinOp relBinOp) {
        traverseChildren(relBinOp);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ArithBinOp arithBinOp) {
        traverseChildren(arithBinOp);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.Unpack unpack) {
        traverseChildren(unpack);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.DotAccess dotAccess) {
        traverseChildren(dotAccess);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SafeAccess safeAccess) {
        traverseChildren(safeAccess);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.InClause inClause) {
        traverseChildren(inClause);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.Indexing indexing) {
        traverseChildren(indexing);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SafeIndexing safeIndexing) {
        traverseChildren(safeIndexing);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.FilteredPattern filteredPattern) {
        traverseChildren(filteredPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BoolPatternFalse boolPatternFalse) {
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BoolPatternTrue boolPatternTrue) {
        return null;
    }

    @Override
    public Void visit(Liblkqllang.IntegerPattern integerPattern) {
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ListPattern listPattern) {
        traverseChildren(listPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SplatPattern splatPattern) {
        if (splatPattern.fBinding() != null) {
            final String symbol = splatPattern.fBinding().getText();
            this.scriptFramesBuilder.addBinding(symbol);
        }
        return null;
    }

    @Override
    public Void visit(Liblkqllang.NullPattern nullPattern) {
        traverseChildren(nullPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ObjectPattern objectPattern) {
        traverseChildren(objectPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.UniversalPattern universalPattern) {
        traverseChildren(universalPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.RegexPattern regexPattern) {
        traverseChildren(regexPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.TuplePattern tuplePattern) {
        traverseChildren(tuplePattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ParenPattern parenPattern) {
        traverseChildren(parenPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.OrPattern orPattern) {
        traverseChildren(orPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.NotPattern notPattern) {
        traverseChildren(notPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.QueryKindAll queryKindAll) {
        traverseChildren(queryKindAll);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.QueryKindFirst queryKindFirst) {
        traverseChildren(queryKindFirst);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ListCompAssoc listCompAssoc) {
        traverseChildren(listCompAssoc);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ListCompAssocList listCompAssocList) {
        traverseChildren(listCompAssocList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.LkqlNodeList lkqlNodeList) {
        traverseChildren(lkqlNodeList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ListLiteral listLiteral) {
        traverseChildren(listLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ObjectLiteral objectLiteral) {
        traverseChildren(objectLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ObjectAssoc objectAssoc) {
        traverseChildren(objectAssoc);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ObjectPatternAssoc objectPatternAssoc) {
        traverseChildren(objectPatternAssoc);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.AtObjectAssoc atObjectAssoc) {
        traverseChildren(atObjectAssoc);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.AtObjectAssocList atObjectAssocList) {
        traverseChildren(atObjectAssocList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BasePatternList basePatternList) {
        traverseChildren(basePatternList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.AtObjectLiteral atObjectLiteral) {
        traverseChildren(atObjectLiteral);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ObjectAssocList objectAssocList) {
        traverseChildren(objectAssocList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BlockBodyList blockBodyList) {
        traverseChildren(blockBodyList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BlockBodyDecl blockBodyDecl) {
        traverseChildren(blockBodyDecl);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.BlockBodyExpr blockBodyExpr) {
        traverseChildren(blockBodyExpr);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SafePresent safePresent) {
        traverseChildren(safePresent);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SafeAbsent safeAbsent) {
        traverseChildren(safeAbsent);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.FunCall funCall) {
        traverseChildren(funCall);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SelectorExprModeDefault selectorExprModeDefault) {
        traverseChildren(selectorExprModeDefault);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SelectorExprModeRec selectorExprModeRec) {
        traverseChildren(selectorExprModeRec);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SelectorExprModeSkip selectorExprModeSkip) {
        traverseChildren(selectorExprModeSkip);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SelectorExpr selectorExpr) {
        traverseChildren(selectorExpr);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SelectorExprList selectorExprList) {
        traverseChildren(selectorExprList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.SelectorArmList selectorArmList) {
        traverseChildren(selectorArmList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.NodeKindPattern nodeKindPattern) {
        traverseChildren(nodeKindPattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.NodePatternDetailList nodePatternDetailList) {
        traverseChildren(nodePatternDetailList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.NodePatternField nodePatternField) {
        traverseChildren(nodePatternField);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.NodePatternProperty nodePatternProperty) {
        traverseChildren(nodePatternProperty);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.ExtendedNodePattern extendedNodePattern) {
        traverseChildren(extendedNodePattern);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.MatchArmList matchArmList) {
        traverseChildren(matchArmList);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.Match match) {
        traverseChildren(match);
        return null;
    }

    @Override
    public Void visit(Liblkqllang.Tuple tuple) {
        traverseChildren(tuple);
        return null;
    }
}
