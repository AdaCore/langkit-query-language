/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.parser;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.nodes.expressions.*;
import com.adacore.lkql_jit.nodes.expressions.literals.*;
import com.adacore.lkql_jit.nodes.expressions.literals.object.ObjectAssoc;
import com.adacore.lkql_jit.nodes.expressions.literals.object.ObjectAssocList;
import com.adacore.lkql_jit.nodes.expressions.literals.object.ObjectLiteral;
import com.adacore.lkql_jit.nodes.expressions.match.MatchArmNodeGen;
import com.adacore.lkql_jit.nodes.expressions.operators.*;
import com.adacore.lkql_jit.nodes.patterns.*;
import com.adacore.lkql_jit.nodes.patterns.chained_patterns.*;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.*;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.source.Source;
import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.ExprArg;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.nodes.declarations.DeclAnnotation;
import com.adacore.lkql_jit.nodes.declarations.Declaration;
import com.adacore.lkql_jit.nodes.declarations.Import;
import com.adacore.lkql_jit.nodes.declarations.ParameterDecl;
import com.adacore.lkql_jit.nodes.declarations.functions.FunDeclGlobal;
import com.adacore.lkql_jit.nodes.declarations.functions.FunDeclLocal;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorArm;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorDeclGlobal;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorDeclLocal;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorExpr;
import com.adacore.lkql_jit.nodes.declarations.variables.ValDeclGlobal;
import com.adacore.lkql_jit.nodes.declarations.variables.ValDeclLocal;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBody;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyDecl;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockBodyExpr;
import com.adacore.lkql_jit.nodes.expressions.block_expression.BlockExpr;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.dot.SafeDotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ListCompAssoc;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ListCompAssocList;
import com.adacore.lkql_jit.nodes.expressions.list_comprehension.ListComprehension;
import com.adacore.lkql_jit.nodes.expressions.match.Match;
import com.adacore.lkql_jit.nodes.expressions.match.MatchArm;
import com.adacore.lkql_jit.nodes.expressions.variables.ReadGlobal;
import com.adacore.lkql_jit.nodes.expressions.variables.ReadLocal;
import com.adacore.lkql_jit.nodes.expressions.variables.ReadVariable;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFactory;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


/**
 * This class translate the langkit AST to a Truffle AST
 * This class rely on the BasicVisitorInterface from the langkit Java bindings
 *
 * @author Hugo GUERRIER
 */
public final class ASTTranslator implements Liblkqllang.BasicVisitor<LKQLNode> {

    // ----- Attributes -----

    /** The current scope for the lexical analysis */
    private final Scope scope;

    /** The source of the AST to translate */
    private final Source source;

    // ----- Constructors -----

    /**
     * Create a new AST translator
     *
     * @param source The source of the AST to translate
     */
    public ASTTranslator(
            Source source
    ) {
        this.scope = new Scope();
        this.source = source;
        BuiltInFactory.getInstance().addBuiltIns(this.scope);
    }

    // ----- Getters -----

    public Scope getScope() {
        return this.scope;
    }

    // ----- Class methods -----

    /**
     * Get the string content from a string literal of langkit
     *
     * @param literal The string literal
     * @return The string content of it
     */
    private static String parseStringLiteral(Liblkqllang.BaseStringLiteral literal) {
        // Prepare the result
        String res = "";

        // If the string literal is a simple on
        if(literal instanceof Liblkqllang.StringLiteral) {
            String raw = literal.getText();
            res = StringUtils.translateEscapes(raw.substring(1, raw.length() - 1));
        }

        // If the string literal is a block
        else if(literal instanceof Liblkqllang.BlockStringLiteral blockStringLiteral) {
            StringBuilder builder = new StringBuilder();
            try(Liblkqllang.LkqlNodeArray nodeArray = blockStringLiteral.fDocs().children()) {
                for(Liblkqllang.LkqlNode node : nodeArray) {
                    builder.append(StringUtils.translateEscapes(node.getText().substring(2))).append("\n");
                }
            }
            res = builder.toString().replaceAll("^\\s+", "");
        }

        // Return the result
        return res;
    }

    // ----- Node translation methods -----


    /**
     * Visit a base node from the LKQL langkit parsing, this should not happen
     *
     * @param lkqlNode The node to visit
     * @return A null reference because this should not happen
     */
    @Override
    public LKQLNode visit(Liblkqllang.LkqlNode lkqlNode) {
        System.err.println("Visiting a base LKQL langkit node, this should not happen !");
        return null;
    }

    /**
     * Visit the top level node list, this what all LKQL programs start with
     *
     * @param topLevelList The base TopLevelList node from langkit
     * @return The TopLevelList node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.TopLevelList topLevelList) {
        // Initialize the LKQL node list
        List<LKQLNode> nodeList = new ArrayList<>();

        // Get the LKQL node list from the top level children
        try(Liblkqllang.LkqlNodeArray array = topLevelList.children()) {
            for(Liblkqllang.LkqlNode node : array) {
                nodeList.add(node.accept(this));
            }
        }

        // Return the top level list
        return new TopLevelList(
                new SourceLocation(this.source, topLevelList.getSourceLocationRange()),
                this.scope.getGlobalSize(),
                this.scope.getExportNumber(),
                nodeList.toArray(new LKQLNode[0])
                );
    }


    // --- Declarations

    /**
     * Visit a declaration annotation
     *
     * @param declAnnotation The base DeclAnnotation node from langkit
     * @return The DeclAnnotation node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.DeclAnnotation declAnnotation) {
        // Translate the annotation fields
        String name = declAnnotation.fName().getText();
        ArgList arguments = (ArgList) declAnnotation.fArguments().accept(this);

        // Return the new declaration annotation node
        return new DeclAnnotation(
                new SourceLocation(this.source, declAnnotation.getSourceLocationRange()),
                name,
                arguments
        );
    }

    /**
     * Visit a function declaration
     *
     * @param funDecl The base FunDecl node from langkit
     * @return The FunDecl node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.FunDecl funDecl) {
        // Translate the declaration fields
        Liblkqllang.DeclAnnotation annotationBase = funDecl.fAnnotation();
        DeclAnnotation annotation = annotationBase == null ?
                null :
                (DeclAnnotation) annotationBase.accept(this);
        String name = funDecl.fName().getText();

        // Get the current slot of the name
        int slot = this.scope.getVariableInScope(name);

        // Verify that the slot is empty
        if(slot > -1) {
            throw LKQLRuntimeException.existingSymbol(
                    name,
                    new DummyLocation(new SourceLocation(this.source, funDecl.getSourceLocationRange()))
            );
        }

        // Add the variable in the scope
        slot = this.scope.addVariable(name);

        // Translate the function body
        FunExpr funExpr = (FunExpr) funDecl.fFunExpr().accept(this);

        // If the function declaration is global
        if(this.scope.isGlobal()) {
            // Return a new function global declaration
            return new FunDeclGlobal(
                    new SourceLocation(this.source, funDecl.getSourceLocationRange()),
                    annotation,
                    name,
                    slot,
                    funExpr
            );
        }

        // If the function declaration is in a local scope
        else {
            // Return the function local declaration
            return new FunDeclLocal(
                    new SourceLocation(this.source, funDecl.getSourceLocationRange()),
                    annotation,
                    name,
                    slot,
                    funExpr
            );
        }
    }

    /**
     * Visit a variable declaration node
     *
     * @param valDecl The base ValDecl node from langkit
     * @return The ValDecl node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ValDecl valDecl) {
        // Translate the declaration fields
        Liblkqllang.DeclAnnotation annotationBase = valDecl.fAnnotation();
        DeclAnnotation annotation = annotationBase == null ?
                null :
                (DeclAnnotation) annotationBase.accept(this);
        String name = valDecl.fIdentifier().getText();
        Expr value = (Expr) valDecl.fValue().accept(this);

        // Get the slot for the name
        int slot = this.scope.getVariableInScope(name);

        // Verify that the slot is empty
        if(slot > -1) {
            throw LKQLRuntimeException.existingSymbol(
                    name,
                    new DummyLocation(new SourceLocation(this.source, valDecl.getSourceLocationRange()))
            );
        }

        // Add the variable in the scope
        slot = this.scope.addVariable(name);

        // If the variable declaration is global
        if(this.scope.isGlobal()) {
            // Return a global variable declaration
            return new ValDeclGlobal(
                    new SourceLocation(this.source, valDecl.getSourceLocationRange()),
                    annotation,
                    name,
                    slot,
                    value
            );
        }

        // If the variable declaration is local
        else {
            // Return the local variable declaration
            return new ValDeclLocal(
                    new SourceLocation(this.source, valDecl.getSourceLocationRange()),
                    annotation,
                    name,
                    slot,
                    value
            );
        }
    }

    /**
     * Visit a import node
     *
     * @param anImport The base Import node from langkit
     * @return The Import node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.Import anImport) {
        // Get the name of the import
        String name = anImport.fName().getText();

        // Create a new global slot for the import
        int slot = this.scope.getVariable(name);

        // Verify the slot emptiness
        if(slot > -1) {
            throw LKQLRuntimeException.existingSymbol(
                    name,
                    new DummyLocation(new SourceLocation(this.source, anImport.getSourceLocationRange()))
            );
        }

        // Create a new global slot
        slot = this.scope.addVariable(name);

        // Return the import node
        return new Import(
                new SourceLocation(this.source, anImport.getSourceLocationRange()),
                name,
                slot
        );
    }


    // --- Functions

    /**
     * Visit a named function node
     *
     * @param namedFunction The base NamedFunction node from langkit
     * @return The FunctionExpr node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NamedFunction namedFunction) {
        return this.functionExprHelper(namedFunction.fParameters(), namedFunction.fBodyExpr());
    }

    /**
     * Visit an anonymous function node
     *
     * @param anonymousFunction The base AnonymousFunction node from langkit
     * @return The FunctionExpr node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.AnonymousFunction anonymousFunction) {
        return this.functionExprHelper(anonymousFunction.fParameters(), anonymousFunction.fBodyExpr());
    }

    /**
     * This function factorize the function expression visit
     *
     * @param parameterList The function parameter list node from langkit
     * @param bodyExpr The function body from langkit
     * @return The FunctionExpr node for Truffle
     */
    private FunExpr functionExprHelper(Liblkqllang.ParameterDeclList parameterList, Liblkqllang.Expr bodyExpr) {
        // Open a new scope
        this.scope.openSemantic();

        // Translate the named function fields
        List<ParameterDecl> parameters = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = parameterList.children()) {
            for(Liblkqllang.LkqlNode param : array) {
                parameters.add((ParameterDecl) param.accept(this));
            }
        }

        Expr body = (Expr) bodyExpr.accept(this);

        // Create the new function expression
        FunExpr res = new FunExpr(
                new SourceLocation(this.source, bodyExpr.getSourceLocationRange()),
                this.scope.buildDescriptor(),
                this.scope.getClosureLimit(),
                parameters.toArray(new ParameterDecl[0]),
                body
        );

        // Close the local scope
        this.scope.closeSemantic();

        // Return the result
        return res;
    }

    /**
     * Visit a function call node
     *
     * @param funCall The base FunCall node from langkit
     * @return The FunCall node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.FunCall funCall) {
        // Translate the function call fields
        Expr callee = (Expr) funCall.fName().accept(this);
        boolean isSafe = funCall.fHasSafe() instanceof Liblkqllang.SafePresent;
        ArgList arguments = (ArgList) funCall.fArguments().accept(this);

        // Return the function call
        return FunCallNodeGen.create(
                new SourceLocation(this.source, funCall.getSourceLocationRange()),
                isSafe,
                new DummyLocation(callee.getLocation()),
                arguments,
                callee
        );
    }


    // --- Parameters

    /**
     * Visit a parameter declaration node
     *
     * @param parameterDecl The base ParameterDecl node from langkit
     * @return The ParameterDecl node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ParameterDecl parameterDecl) {
        // Translate the parameter declaration fields
        String name = parameterDecl.fParamIdentifier().getText();
        Liblkqllang.Identifier typeAnnotationBase = parameterDecl.fTypeAnnotation();
        String typeAnnotation = typeAnnotationBase == null ?
                null :
                typeAnnotationBase.getText();
        Liblkqllang.Expr parameterDeclBase = parameterDecl.fDefaultExpr();
        Expr defaultValue = parameterDeclBase == null ?
                null :
                (Expr) parameterDeclBase.accept(this);

        // Get the frame slot of the parameter
        int slot = this.scope.addVariable(name);

        // Return the new parameter declaration
        return new ParameterDecl(
                new SourceLocation(this.source, parameterDecl.getSourceLocationRange()),
                name,
                slot,
                typeAnnotation,
                defaultValue
        );
    }


    // --- Arguments

    /**
     * Visit an expression argument node
     *
     * @param exprArg The base ExprArg node from langkit
     * @return The ExprArg node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ExprArg exprArg) {
        // Translate the argument fields
        Expr argExpr = (Expr) exprArg.fValueExpr().accept(this);

        // Return the new expression argument node
        return new ExprArg(
                new SourceLocation(this.source, exprArg.getSourceLocationRange()),
                argExpr
        );
    }

    /**
     * Visit a named argument node
     *
     * @param namedArg The base NamedArg node from langkit
     * @return The NamedArg node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NamedArg namedArg) {
        // Translate the argument fields
        String name = namedArg.fArgName().getText();
        Expr argExpr = (Expr) namedArg.fValueExpr().accept(this);

        // Return the new named argument
        return new NamedArg(
                new SourceLocation(this.source, namedArg.getSourceLocationRange()),
                new Identifier(
                        new SourceLocation(this.source, namedArg.fArgName().getSourceLocationRange()),
                        name
                ),
                argExpr
        );
    }

    /**
     * Visit a synthetic named argument node
     *
     * @param synthNamedArg The base SynthNamedArg node from langkit
     * @return The NamedArg node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.SynthNamedArg synthNamedArg) {
        // For now, do the exact same thing as a simple named argument
        return this.visit((Liblkqllang.NamedArg) synthNamedArg);
    }

    /**
     * Visit an argument list node
     *
     * @param argList The base ArgList node from langkit
     * @return The ArgList node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ArgList argList) {
        // Prepare the argument list
        List<Arg> arguments = new ArrayList<>();

        // Set the working vars
        boolean namedPhase = false;
        Set<String> namedSeen = new HashSet<>();

        // Get the arguments from the langkit node
        try(Liblkqllang.LkqlNodeArray array = argList.children()) {
            for(Liblkqllang.LkqlNode arg : array) {
                Arg curArg = (Arg) arg.accept(this);

                // Verify the argument phase
                if(curArg instanceof ExprArg) {
                    if(namedPhase) {
                        throw LKQLRuntimeException.positionAfterNamedArgument(curArg);
                    }
                }

                // Verify the same name arguments
                else if (curArg instanceof NamedArg namedArg) {
                    namedPhase = true;
                    if(namedSeen.contains(namedArg.getArgName().getName())) {
                        throw LKQLRuntimeException.multipleSameNameArgument(curArg);
                    }
                    namedSeen.add(namedArg.getArgName().getName());
                }

                // Add the argument to the list
                arguments.add(curArg);
            }
        }

        // Return the new argument list node
        return new ArgList(
                new SourceLocation(this.source, argList.getSourceLocationRange()),
                arguments.toArray(new Arg[0])
        );
    }

    // --- Expressions

    /**
     * Visit a block expression node
     *
     * @param blockExpr The base BlockExpr node from langkit
     * @return The BlockExpr node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.BlockExpr blockExpr) {
        // Open a new scope
        this.scope.openLexical();

        // Create the declaration list
        List<BlockBody> blockBody = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = blockExpr.fBody().children()) {
            for(Liblkqllang.LkqlNode bodyPart : array) {
                blockBody.add((BlockBody) bodyPart.accept(this));
            }
        }

        // Get the expression of the block
        Expr expr = (Expr) blockExpr.fExpr().accept(this);

        // Create the result
        BlockExpr res = new BlockExpr(
                new SourceLocation(this.source, blockExpr.getSourceLocationRange()),
                blockBody.toArray(new BlockBody[0]),
                expr
        );

        // Close the local scope
        this.scope.closeLexical();

        // Return the result
        return res;
    }

    /**
     * Visit a block body part declaration node
     *
     * @param blockBodyDecl The base BlockBodyDecl node from langkit
     * @return The Declaration node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.BlockBodyDecl blockBodyDecl) {
        return new BlockBodyDecl(
                new SourceLocation(this.source, blockBodyDecl.getSourceLocationRange()),
                (Declaration) blockBodyDecl.fDecl().accept(this)
        );
    }

    /**
     * Visit a block body part expression node
     *
     * @param blockBodyExpr The base BlockBodyExpr node from langkit
     * @return The Expr node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.BlockBodyExpr blockBodyExpr) {
        return new BlockBodyExpr(
                new SourceLocation(this.source, blockBodyExpr.getSourceLocationRange()),
                (Expr) blockBodyExpr.fExpr().accept(this)
        );
    }

    /**
     * Visit a list comprehension node
     *
     * @param listComprehension The base ListComprehension node from langkit
     * @return The ListComprehension node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ListComprehension listComprehension) {
        // Translate the list comprehension collections
        List<Expr> collections = new ArrayList<>();
        Liblkqllang.ListCompAssocList assocListOrigin = listComprehension.fGenerators();
        for(int i = 0 ; i < assocListOrigin.getChildrenCount() ; i++) {
            Liblkqllang.ListCompAssoc assocOrigin = (Liblkqllang.ListCompAssoc) assocListOrigin.getChild(i);
            collections.add((Expr) assocOrigin.fCollExpr().accept(this));
        }

        // Open a new scope
        this.scope.openSemantic();

        // Create the list comprehension associations
        List<ListCompAssoc> assocList = new ArrayList<>();
        for(int i = 0 ; i < assocListOrigin.getChildrenCount() ; i++) {
            Liblkqllang.ListCompAssoc assocOrigin = (Liblkqllang.ListCompAssoc) assocListOrigin.getChild(i);
            String name = assocOrigin.fBindingName().getText();
            int slot = this.scope.addVariable(name);

            assocList.add(new ListCompAssoc(
                    new SourceLocation(this.source, assocOrigin.getSourceLocationRange()),
                    name,
                    slot,
                    collections.get(i)
            ));
        }

        // Translate the list comprehension fields
        Expr expr = (Expr) listComprehension.fExpr().accept(this);
        Liblkqllang.Expr guardBase = listComprehension.fGuard();
        Expr guard = guardBase == null ?
                null :
                (Expr) guardBase.accept(this);

        // Create the result
        ListComprehension res = new ListComprehension(
                new SourceLocation(this.source, listComprehension.getSourceLocationRange()),
                this.scope.buildDescriptor(),
                this.scope.getClosureLimit(),
                new ListCompAssocList(
                        new SourceLocation(this.source, assocListOrigin.getSourceLocationRange()),
                        assocList.toArray(new ListCompAssoc[0])
                ),
                expr,
                guard
        );

        // Close the local scope
        this.scope.closeSemantic();

        // Return the result
        return res;
    }

    @Override
    public LKQLNode visit(Liblkqllang.ListCompAssoc listCompAssoc) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.ListCompAssocList listCompAssocList) { return null; }

    /**
     * Visit a conditional node
     *
     * @param ifThenElse The base IfThenElse node from langkit
     * @return The IfThenElse node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.IfThenElse ifThenElse) {
        // Translate the if then else fields
        Expr condition = (Expr) ifThenElse.fCondition().accept(this);
        Expr consequence = (Expr) ifThenElse.fThenExpr().accept(this);
        Expr alternative = (Expr) ifThenElse.fElseExpr().accept(this);

        // Return the if then else node
        return new IfThenElse(
                new SourceLocation(this.source, ifThenElse.getSourceLocationRange()),
                condition,
                consequence,
                alternative
        );
    }

    /**
     * Visit a indexing node
     *
     * @param indexing The base Indexing node from langkit
     * @return The Indexing node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.Indexing indexing) {
        // Translate the indexing fields
        Expr collection = (Expr) indexing.fCollectionExpr().accept(this);
        Expr index = (Expr) indexing.fIndexExpr().accept(this);

        // Return the indexing node
        return IndexingNodeGen.create(
                new SourceLocation(this.source, indexing.getSourceLocationRange()),
                false,
                collection,
                index
        );
    }

    /**
     * Visit a safe indexing node
     *
     * @param safeIndexing The base SafeIndexing node from langkit
     * @return The Indexing node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.SafeIndexing safeIndexing) {
        // Translate the safe indexing fields
        Expr collection = (Expr) safeIndexing.fCollectionExpr().accept(this);
        Expr index = (Expr) safeIndexing.fIndexExpr().accept(this);

        // Return the indexing node with safe flag
        return IndexingNodeGen.create(
                new SourceLocation(this.source, safeIndexing.getSourceLocationRange()),
                true,
                collection,
                index
        );
    }

    /**
     * Visit a dot access node
     *
     * @param dotAccess The base DotAccess node from langkit
     * @return The DotAccess node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.DotAccess dotAccess) {
        // Translate the dot access fields
        Liblkqllang.Identifier memberIdentifier = dotAccess.fMember();
        Identifier member = new Identifier(
                new SourceLocation(this.source, memberIdentifier.getSourceLocationRange()),
                memberIdentifier.getText()
        );
        Expr receiver = (Expr) dotAccess.fReceiver().accept(this);

        // Return the new DotAccess node
        return DotAccessNodeGen.create(
                new SourceLocation(this.source, dotAccess.getSourceLocationRange()),
                member,
                receiver
        );
    }

    /**
     * Visit a safe dot access node
     *
     * @param safeAccess The base SafeAccess node from langkit
     * @return The SafeAccess node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.SafeAccess safeAccess) {
        // Translate the dot access fields
        Liblkqllang.Identifier memberIdentifier = safeAccess.fMember();
        Identifier member = new Identifier(
                new SourceLocation(this.source, memberIdentifier.getSourceLocationRange()),
                memberIdentifier.getText()
        );
        Expr receiver = (Expr) safeAccess.fReceiver().accept(this);

        // Return the new DotAccess node
        return SafeDotAccessNodeGen.create(
                new SourceLocation(this.source, safeAccess.getSourceLocationRange()),
                member,
                receiver
        );
    }

    /**
     * Visit an unpack node
     *
     * @param unpack The base Unpack node from langkit
     * @return The Unpack node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.Unpack unpack) {
        // Translate the unpack field
        Expr collectionExpr = (Expr) unpack.fCollectionExpr().accept(this);

        // Return the new unpack node
        return new Unpack(
                new SourceLocation(this.source, unpack.getSourceLocationRange()),
                collectionExpr
        );
    }

    /**
     * Visit an unwrap node
     *
     * @param unwrap The base Unwrap node from langkit
     * @return The Unwrap node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.Unwrap unwrap) {
        // Translate the unwrap field
        Expr nodeExpr = (Expr) unwrap.fNodeExpr().accept(this);

        // Return the new unwrap node
        return new Unwrap(
                new SourceLocation(this.source, unwrap.getSourceLocationRange()),
                nodeExpr
        );
    }

    /**
     * Visit a match node
     *
     * @param match The base Match node from langkit
     * @return The Match node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.Match match) {
        // Translate the match fields
        Expr matchVal = (Expr) match.fMatchedVal().accept(this);

        // Get the match arms
        List<MatchArm> arms = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = match.fArms().children()) {
            for(Liblkqllang.LkqlNode node : array) {
                // Open a lexical scope for the arm
                this.scope.openLexical();

                // Visit the arm
                arms.add((MatchArm) node.accept(this));

                // Close the lexical scope
                this.scope.closeLexical();
            }
        }

        // Return a new match node
        return new Match(
                new SourceLocation(this.source, match.getSourceLocationRange()),
                matchVal,
                arms.toArray(new MatchArm[0])
        );
    }

    /**
     * Visit a match arm node
     *
     * @param matchArm The base MatchArm node from langkit
     * @return The MatchArm node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.MatchArm matchArm) {
        // Translate the match arm fields
        BasePattern pattern = (BasePattern) matchArm.fPattern().accept(this);
        Expr expr = (Expr) matchArm.fExpr().accept(this);

        // Return the new match arm
        return MatchArmNodeGen.create(
                new SourceLocation(this.source, matchArm.getSourceLocationRange()),
                pattern,
                expr
        );
    }


    // --- Unary operators

    /**
     * Visit a unary operation node
     *
     * @param unOp The base UnOp node from langkit
     * @return The UnOp node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.UnOp unOp) {
        // Translate the unary operator argument
        Expr arg = (Expr) unOp.fOperand().accept(this);

        DummyLocation argLocation = new DummyLocation(arg.getLocation());
        SourceLocation location = new SourceLocation(this.source, unOp.getSourceLocationRange());

        // Create the unary operator by switching on the operator type
        return switch (unOp.fOp().getKindName()) {
            case Liblkqllang.OpNot.kindName -> UnNotNodeGen.create(location, argLocation, arg);
            case Liblkqllang.OpPlus.kindName -> UnPlusNodeGen.create(location, argLocation, arg);
            case Liblkqllang.OpMinus.kindName -> UnMinusNodeGen.create(location, argLocation, arg);
            default -> null;
        };
    }


    // --- Binary operators

    /**
     * Visit a binary operation node
     *
     * @param binOp The base BinOp node from langkit
     * @return The BinOp node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.BinOp binOp) {
        // Translate the binary operands
        Expr left = (Expr) binOp.fLeft().accept(this);
        Expr right = (Expr) binOp.fRight().accept(this);

        DummyLocation leftLocation = new DummyLocation(left.getLocation());
        DummyLocation rightLocation = new DummyLocation(right.getLocation());
        SourceLocation location = new SourceLocation(this.source, binOp.getSourceLocationRange());

        // Create the binary operator by switching on the operator type
        return switch (binOp.fOp().getKindName()) {
            case Liblkqllang.OpPlus.kindName -> BinPlusNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpMinus.kindName -> BinMinusNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpMul.kindName -> BinMulNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpDiv.kindName -> BinDivNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpAnd.kindName -> new BinAnd(location, left, right);
            case Liblkqllang.OpOr.kindName -> new BinOr(location, left, right);
            case Liblkqllang.OpEq.kindName -> BinEqNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpNeq.kindName -> BinNeqNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpConcat.kindName -> BinConcatNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpLt.kindName -> BinLtNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpLeq.kindName -> BinLeqNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpGt.kindName -> BinGtNodeGen.create(location, leftLocation, rightLocation, left, right);
            case Liblkqllang.OpGeq.kindName -> BinGeqNodeGen.create(location, leftLocation, rightLocation, left, right);
            default -> null;
        };
    }

    /**
     * Visit an arithmetic binary operation node
     *
     * @param arithBinOp The base ArithBinOp node from langkit
     * @return The BinOp node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ArithBinOp arithBinOp) {
        // Pass the arithmetic operation to the general binary operation visitor
        return this.visit((Liblkqllang.BinOp) arithBinOp);
    }

    /**
     * Visit a relational binary operation node
     *
     * @param relBinOp The base RelBinOp node from langkit
     * @return The BinOp node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.RelBinOp relBinOp) {
        // Pass the arithmetic operation to the general binary operation visitor
        return this.visit((Liblkqllang.BinOp) relBinOp);
    }

    /**
     * Visit a "in" clause node
     *
     * @param inClause The base InClause node from langkit
     * @return The InClause node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.InClause inClause) {
        // Translate the in clause fields
        Expr elem = (Expr) inClause.fValueExpr().accept(this);
        Expr indexable = (Expr) inClause.fListExpr().accept(this);

        // Return the new in node
        return InClauseNodeGen.create(
                new SourceLocation(this.source, inClause.getSourceLocationRange()),
                new DummyLocation(elem.getLocation()),
                new DummyLocation(indexable.getLocation()),
                elem,
                indexable
        );
    }

    /**
     * Visit a "is" clause node
     *
     * @param isClause The base IsClause node from langkit
     * @return The IsClause node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.IsClause isClause) {
        // Open a lexical scope
        this.scope.openLexical();

        // Translate the is clause fields
        Expr nodeExpr = (Expr) isClause.fNodeExpr().accept(this);
        BasePattern pattern = (BasePattern) isClause.fPattern().accept(this);

        // Create the result
        IsClause res = IsClauseNodeGen.create(
                new SourceLocation(this.source, isClause.getSourceLocationRange()),
                new DummyLocation(nodeExpr.getLocation()),
                pattern,
                nodeExpr
        );

        // Close the lexical scope
        this.scope.closeLexical();

        // Return the result
        return res;
    }


    // --- Literal nodes

    /**
     * Visit a null node
     *
     * @param nullLiteral The base NullLiteral node from langkit
     * @return The NullLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NullLiteral nullLiteral) {
        return new NullLiteral(new SourceLocation(this.source, nullLiteral.getSourceLocationRange()));
    }

    /**
     * Visit a unit node
     *
     * @param unitLiteral The base UnitLiteral node from langkit
     * @return The UnitLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.UnitLiteral unitLiteral) {
        return new UnitLiteral(new SourceLocation(this.source, unitLiteral.getSourceLocationRange()));
    }

    /**
     * Visit a false boolean node
     *
     * @param boolLiteralFalse The base BoolLiteralFalse node from langkit
     * @return The BooleanLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.BoolLiteralFalse boolLiteralFalse) {
        return new BooleanLiteral(
                new SourceLocation(this.source, boolLiteralFalse.getSourceLocationRange()),
                false
        );
    }

    /**
     * Visit a true boolean node
     *
     * @param boolLiteralTrue The base BoolLiteralTrue node from langkit
     * @return The BooleanLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.BoolLiteralTrue boolLiteralTrue) {
        return new BooleanLiteral(
                new SourceLocation(this.source, boolLiteralTrue.getSourceLocationRange()),
                true
        );
    }

    /**
     * Visit a integer node
     *
     * @param integerLiteral The base IntegerLiteral node from langkit
     * @return The IntegerLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.IntegerLiteral integerLiteral) {
        try {
            long longValue = Long.parseLong(integerLiteral.getText());
            return new LongLiteral(
                    new SourceLocation(this.source, integerLiteral.getSourceLocationRange()),
                    longValue
            );
        } catch (NumberFormatException e) {
            return new BigIntegerLiteral(
                    new SourceLocation(this.source, integerLiteral.getSourceLocationRange()),
                    new BigInteger(integerLiteral.getText())
            );
        }
    }

    /**
     * Visit a string node
     *
     * @param stringLiteral The base StringLiteral node from langkit
     * @return The StringLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.StringLiteral stringLiteral) {
        // Return the string literal token
        return new StringLiteral(
                new SourceLocation(this.source, stringLiteral.getSourceLocationRange()),
                parseStringLiteral(stringLiteral)
        );
    }

    /**
     * Visit a block string node
     *
     * @param blockStringLiteral The base BlockStringLiteral node from langkit
     * @return The BlockStringLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.BlockStringLiteral blockStringLiteral) {
        // Return the new string literal
        return new StringLiteral(
                new SourceLocation(this.source, blockStringLiteral.getSourceLocationRange()),
                parseStringLiteral(blockStringLiteral)
        );
    }

    /**
     * Visit an identifier node
     *
     * @param identifier The base Identifier node from langkit
     * @return The Identifier node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.Identifier identifier) {
        // Get the identifier string
        String name = identifier.getText();

        // Get the identifier slot
        int slot = this.scope.getVariable(name);

        // Verify that the variable exists
        if(slot < 0) {
            throw LKQLRuntimeException.unknownSymbol(
                    name,
                    new DummyLocation(new SourceLocation(this.source, identifier.getSourceLocationRange()))
            );
        }

        // If the variable is local
        if(this.scope.isSemanticLocal(name)) {
            return new ReadLocal(
                    new SourceLocation(this.source, identifier.getSourceLocationRange()),
                    slot
            );
        }

        // Else, the variable is global
        else {
            return new ReadGlobal(
                    new SourceLocation(this.source, identifier.getSourceLocationRange()),
                    slot
            );
        }
    }

    /**
     * Visit a tuple literal node
     *
     * @param tuple The base Tuple node from langkit
     * @return The TupleLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.Tuple tuple) {
        // Create the expression array
        List<Expr> tupleExprs = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = tuple.fExprs().children()) {
            for(Liblkqllang.LkqlNode expr : array) {
                tupleExprs.add((Expr) expr.accept(this));
            }
        }

        // Return the tuple literal node
        return new TupleLiteral(
                new SourceLocation(this.source, tuple.getSourceLocationRange()),
                tupleExprs.toArray(new Expr[0])
        );
    }

    /**
     * Visit a list literal node
     *
     * @param listLiteral The base ListLiteral node from langkit
     * @return The ListLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ListLiteral listLiteral) {
        // Create the expression array
        List<Expr> listExprs = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = listLiteral.fExprs().children()) {
            for(Liblkqllang.LkqlNode expr : array) {
                listExprs.add((Expr) expr.accept(this));
            }
        }

        // Return the list literal node
        return new ListLiteral(
                new SourceLocation(this.source, listLiteral.getSourceLocationRange()),
                listExprs.toArray(new Expr[0])
        );
    }

    /**
     * Visit an object literal node
     *
     * @param objectLiteral The base ObjectLiteral node from langkit
     * @return The ObjectLiteral node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ObjectLiteral objectLiteral) {
        // Get the object associations
        ObjectAssocList assocList = (ObjectAssocList) objectLiteral.fAssocs().accept(this);

        // Return the new object literal
        return new ObjectLiteral(
                new SourceLocation(this.source, objectLiteral.getSourceLocationRange()),
                assocList
        );
    }

    /**
     * Visit an object association node
     *
     * @param objectAssoc The base ObjectAssoc node from langkit
     * @return The ObjectAssoc node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ObjectAssoc objectAssoc) {
        // Translate the object association fields
        String name = objectAssoc.fName().getText();
        Expr expr = (Expr) objectAssoc.fExpr().accept(this);

        // Return the object association
        return new ObjectAssoc(
                new SourceLocation(this.source, objectAssoc.getSourceLocationRange()),
                name,
                expr
        );
    }

    /**
     * Visit an object association list node
     *
     * @param objectAssocList The base ObjectAssocList node from langkit
     * @return The ObjectAssocList node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ObjectAssocList objectAssocList) {
        // Prepare the association list
        List<ObjectAssoc> assocList = new ArrayList<>();

        try(Liblkqllang.LkqlNodeArray array = objectAssocList.children()) {
            for(Liblkqllang.LkqlNode assoc : array) {
                assocList.add((ObjectAssoc) assoc.accept(this));
            }
        }

        // Return the object association list
        return new ObjectAssocList(
                new SourceLocation(this.source, objectAssocList.getSourceLocationRange()),
                assocList.toArray(new ObjectAssoc[0])
        );
    }


    // --- Query nodes

    /**
     * Visit a query node
     *
     * @param query The base Query node from langkit
     * @return The Query node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.Query query) {
        // Open a new scope
        this.scope.openLexical();

        // Translate the query fields
        boolean followGenerics = false;
        Liblkqllang.Expr throughExprBase = query.fThroughExpr();
        Expr throughExpr = null;
        if(throughExprBase != null) {
            if(throughExprBase.getText().equals("follow_generics")) followGenerics = true;
            else throughExpr = (Expr) throughExprBase.accept(this);
        }

        Liblkqllang.Expr fromExprBase = query.fFromExpr();
        Expr fromExpr = fromExprBase == null ?
                null :
                (Expr) fromExprBase.accept(this);

        BasePattern pattern = (BasePattern) query.fPattern().accept(this);

        // Get the query kind
        Query.QueryKind queryKind = Query.QueryKind.ALL;
        Liblkqllang.QueryKind queryKindBase = query.fQueryKind();
        if(queryKindBase instanceof Liblkqllang.QueryKindFirst) {
            queryKind = Query.QueryKind.FIRST;
        }

        // Close the local scope
        this.scope.closeLexical();

        // Return the result
        return new Query(
                new SourceLocation(this.source, query.getSourceLocationRange()),
                queryKind,
                followGenerics,
                throughExpr,
                fromExpr,
                pattern
        );
    }


    // --- Selector nodes

    /**
     * Visit a selector call node
     *
     * @param selectorCall The base SelectorCall node from langkit
     * @return The SelectorCall node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorCall selectorCall) {
        // Translate the selector call fields
        SelectorCall.Quantifier quantifier;
        if(selectorCall.fQuantifier().getText().equals("any")) {
            quantifier = SelectorCall.Quantifier.ANY;
        } else {
            quantifier = SelectorCall.Quantifier.ALL;
        }

        Liblkqllang.Identifier bindingBase = selectorCall.fBinding();
        String binding = bindingBase == null ?
                null :
                bindingBase.getText();

        Liblkqllang.Expr selectorExprBase = selectorCall.fSelectorCall();
        Expr selectorExpr;
        ArgList args = null;
        if(selectorExprBase instanceof Liblkqllang.FunCall funCallBase) {
            selectorExpr = (Expr) funCallBase.fName().accept(this);
            args = (ArgList) funCallBase.fArguments().accept(this);
        } else {
            selectorExpr = (Expr) selectorExprBase.accept(this);
        }

        // Get the slot for the binding
        int bindingSlot = -1;
        if(binding != null) {
            bindingSlot = this.scope.getVariableInScope(binding);

            if(bindingSlot > -1) {
                // TODO : Verify that the slot is empty
//                throw LKQLRuntimeException.existingSymbol(
//                        binding,
//                        new DummyLocation(new SourceLocation(this.source, bindingBase.getSourceLocationRange()))
//                );
            } else {
                // Create a new slot for the binding
                bindingSlot = this.scope.addVariable(binding);
            }

        }

        // Return the new node
        return new SelectorCall(
                new SourceLocation(this.source, selectorCall.getSourceLocationRange()),
                quantifier,
                bindingSlot,
                this.scope.isGlobal() ? SelectorCall.Mode.GLOBAL : SelectorCall.Mode.LOCAL,
                selectorExpr,
                args
        );
    }

    /**
     * Visit a selector declaration node
     *
     * @param selectorDecl The base SelectorDecl node from langkit
     * @return The SelectorDecl node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorDecl selectorDecl) {
        // Get the name of the selector
        String name = selectorDecl.fName().getText();
        String documentation = selectorDecl.fDocNode() == null ?
                "" :
                parseStringLiteral(selectorDecl.fDocNode());
        Liblkqllang.DeclAnnotation annotationBase = selectorDecl.fAnnotation();
        DeclAnnotation annotation = annotationBase == null ?
                null :
                (DeclAnnotation) annotationBase.accept(this);

        // Get the current slot of the name
        int slot = this.scope.getVariableInScope(name);

        // Verify that the slot is empty
        if(slot > -1) {
            throw LKQLRuntimeException.existingSymbol(
                    name,
                    new DummyLocation(new SourceLocation(this.source, selectorDecl.getSourceLocationRange()))
            );
        }

        // Open a new scope
        this.scope.openSemantic();

        // Add the "this" local variable
        int thisSlot = this.scope.addVariable("this");

        // Add the "depth" local variable
        int depthSlot = this.scope.addVariable("depth");

        // Get the selector arms
        List<SelectorArm> arms = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = selectorDecl.fArms().children()) {
            for(Liblkqllang.LkqlNode node : array) {
                // Open a lexical scope for the arm
                this.scope.openLexical();

                // Visit the arm
                arms.add((SelectorArm) node.accept(this));

                // Close the lexical scope
                this.scope.closeLexical();
            }
        }

        // Create the frame descriptor for the arms
        FrameDescriptor selectorDescriptor = this.scope.buildDescriptor();
        int closureLimit = this.scope.getClosureLimit();

        // Close the local scope
        this.scope.closeSemantic();

        // Get the slot for the selector value
        slot = this.scope.addVariable(name);

        // If the current scope is global, create a global declaration
        if(this.scope.isGlobal()) {
            return new SelectorDeclGlobal(
                    new SourceLocation(this.source, selectorDecl.getSourceLocationRange()),
                    annotation,
                    name,
                    documentation,
                    slot,
                    thisSlot,
                    depthSlot,
                    selectorDescriptor,
                    arms.toArray(new SelectorArm[0])
            );
        }

        // If the scope is local, create a local selector
        else {
            return new SelectorDeclLocal(
                    new SourceLocation(this.source, selectorDecl.getSourceLocationRange()),
                    annotation,
                    name,
                    documentation,
                    slot,
                    thisSlot,
                    depthSlot,
                    selectorDescriptor,
                    closureLimit,
                    arms.toArray(new SelectorArm[0])
            );
        }
    }

    /**
     * Visit a selector arm node
     *
     * @param selectorArm The base SelectorArm node from langkit
     * @return The SelectorArm node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorArm selectorArm) {
        // Translate the selector arm fields
        BasePattern pattern = (BasePattern) selectorArm.fPattern().accept(this);
        SelectorExpr expr;

        // Get the first element of the expressions
        List<SelectorExpr> exprList = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = selectorArm.fExprsList().children()) {
            for(Liblkqllang.LkqlNode node : array) {
                exprList.add((SelectorExpr) node.accept(this));
            }
        }
        expr = exprList.get(0);

        // Return the selector arm node
        return new SelectorArm(
                new SourceLocation(this.source, selectorArm.getSourceLocationRange()),
                pattern,
                expr
        );
    }

    /**
     * Visit a selector expression node
     *
     * @param selectorExpr The base SelectorExpr node from langkit
     * @return The SelectorExpr node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorExpr selectorExpr) {
        // Translate the selector expression fields
        Expr expr = (Expr) selectorExpr.fExpr().accept(this);

        // Get the expression mode
        SelectorExpr.Mode mode = SelectorExpr.Mode.DEFAULT;
        Liblkqllang.SelectorExprMode modeBase = selectorExpr.fMode();
        if(modeBase instanceof Liblkqllang.SelectorExprModeRec) {
            mode = SelectorExpr.Mode.REC;
        } else if(modeBase instanceof Liblkqllang.SelectorExprModeSkip) {
            mode = SelectorExpr.Mode.SKIP;
        }

        // Create the new selector expression
        return new SelectorExpr(
                new SourceLocation(this.source, selectorExpr.getSourceLocationRange()),
                mode,
                expr
        );
    }


    // --- General pattern nodes

    /**
     * Visit a parented pattern node
     *
     * @param parenPattern The base ParenPattern node from langkit
     * @return The ParenPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ParenPattern parenPattern) {
        return new ParenPattern(
                new SourceLocation(this.source, parenPattern.getSourceLocationRange()),
                (BasePattern) parenPattern.fPattern().accept(this)
        );
    }

    /**
     * Visit a filtered pattern node
     *
     * @param filteredPattern The base FilteredPattern node from langkit
     * @return The FilteredPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.FilteredPattern filteredPattern) {
        // Translate the filtered pattern fields
        UnfilteredPattern pattern = (UnfilteredPattern) filteredPattern.fPattern().accept(this);
        Expr predicate = (Expr) filteredPattern.fPredicate().accept(this);

        // Create the new filtered pattern node
        return new FilteredPattern(
                new SourceLocation(this.source, filteredPattern.getSourceLocationRange()),
                pattern,
                predicate
        );
    }

    /**
     * Visit a binding pattern node
     *
     * @param bindingPattern The base BindingPattern node from langkit
     * @return The BindingPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.BindingPattern bindingPattern) {
        // Get the binding var name and pattern
        String name = bindingPattern.fBinding().getText();

        // Get the slot of the binding
        int slot = this.scope.getVariableInScope(name);

        if(slot > -1) {
            // TODO : Verify that the slot is empty
//            throw LKQLRuntimeException.existingSymbol(
//                    name,
//                    new DummyLocation(new SourceLocation(this.source, bindingPattern.fBinding().getSourceLocationRange()))
//            );
        } else {
            // Add a new slot
            slot = this.scope.addVariable(name);
        }


        // Get the slot for the binding and prepare the mode
        BindingPattern.Mode mode = this.scope.isGlobal() ? BindingPattern.Mode.GLOBAL : BindingPattern.Mode.LOCAL;

        // Visit the value patter
        ValuePattern pattern = (ValuePattern) bindingPattern.fValuePattern().accept(this);

        // Return the result
        return new BindingPattern(
                new SourceLocation(this.source, bindingPattern.getSourceLocationRange()),
                mode,
                slot,
                pattern
        );
    }

    /**
     * Visit a null pattern node
     *
     * @param nullPattern The base NullPattern node from langkit
     * @return The NullPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NullPattern nullPattern) {
        // Just return the null pattern node
        return new NullPattern(
                new SourceLocation(this.source, nullPattern.getSourceLocationRange())
        );
    }

    /**
     * Visit a universal pattern node
     *
     * @param universalPattern The base UniversalPattern node from langkit
     * @return The UniversalPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.UniversalPattern universalPattern) {
        // Just return the universal pattern node
        return new UniversalPattern(
                new SourceLocation(this.source, universalPattern.getSourceLocationRange())
        );
    }

    /**
     * Visit a regex pattern node
     *
     * @param regexPattern The base RegexPattern node from langkit
     * @return The RegexPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.RegexPattern regexPattern) {
        // Get the regex string
        String regex = regexPattern.getText();
        regex = regex.substring(1, regex.length() - 1);

        // Just return the regex pattern node
        return new RegexPattern(
                new SourceLocation(this.source, regexPattern.getSourceLocationRange()),
                regex
        );
    }

    /**
     * Visit an or pattern node
     *
     * @param orPattern The base OrPattern node from langkit
     * @return The OrPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.OrPattern orPattern) {
        // Translate the or pattern fields
        BasePattern left = (BasePattern) orPattern.fLeft().accept(this);
        BasePattern right = (BasePattern) orPattern.fRight().accept(this);

        // Create the new or pattern node
        return new OrPattern(
                new SourceLocation(this.source, orPattern.getSourceLocationRange()),
                left,
                right
        );
    }

    /**
     * Visit a not pattern node
     *
     * @param notPattern The base NotPattern node from langkit
     * @return The NotPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NotPattern notPattern) {
        // Translate the not pattern fields
        ValuePattern pattern = (ValuePattern) notPattern.fPattern().accept(this);

        // Create the new not pattern
        return new NotPattern(
                new SourceLocation(this.source, notPattern.getSourceLocationRange()),
                pattern
        );
    }


    // --- Node pattern nodes

    /**
     * Visit a node kind pattern node
     *
     * @param nodeKindPattern The base NodeKindPattern node from langkit
     * @return The NodeKindPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NodeKindPattern nodeKindPattern) {
        // Just return the new node kind pattern
        return new NodeKindPattern(
                new SourceLocation(this.source, nodeKindPattern.getSourceLocationRange()),
                nodeKindPattern.fKindName().getText()
        );
    }

    /**
     * Visit an extended node kind pattern node
     *
     * @param extendedNodePattern The base ExtendedNodePattern node from langkit
     * @return The ExtendedNodePattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ExtendedNodePattern extendedNodePattern) {
        // Translate the extended node pattern fields
        ValuePattern nodePattern = (ValuePattern) extendedNodePattern.fNodePattern().accept(this);

        // Get the pattern details
        List<NodePatternDetail> details = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = extendedNodePattern.fDetails().children()) {
            for(Liblkqllang.LkqlNode node : array) {
                details.add((NodePatternDetail) node.accept(this));
            }
        }

        // Return the new
        return new ExtendedNodePattern(
                new SourceLocation(this.source, extendedNodePattern.getSourceLocationRange()),
                nodePattern,
                details.toArray(new NodePatternDetail[0])
        );
    }

    /**
     * Visit a detail expression node
     *
     * @param detailExpr The base DetailExpr node from langkit
     * @return The DetailExpr node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.DetailExpr detailExpr) {
        // Translate the detail expression fields
        Expr expr = (Expr) detailExpr.fExprValue().accept(this);

        // Return the new detail expression node
        return new DetailExpr(
                new SourceLocation(this.source, detailExpr.getSourceLocationRange()),
                expr
        );
    }

    /**
     * Visit a detail pattern node
     *
     * @param detailPattern The base DetailPattern node from langkit
     * @return The DetailPattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.DetailPattern detailPattern) {
        // Translate the detail pattern fields
        BasePattern pattern = (BasePattern) detailPattern.fPatternValue().accept(this);

        // Return the new detail pattern node
        return new DetailPattern(
                new SourceLocation(this.source, detailPattern.getSourceLocationRange()),
                pattern
        );
    }

    /**
     * Visit a node pattern field node
     *
     * @param nodePatternField The base NodePatternField node from langkit
     * @return The NodePatternField node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NodePatternField nodePatternField) {
        // Translate the node pattern detail fields
        String name = nodePatternField.fIdentifier().getText();
        DetailValue expected = (DetailValue) nodePatternField.fExpectedValue().accept(this);

        // Return the new node pattern field detail
        return NodePatternFieldNodeGen.create(
                new SourceLocation(this.source, nodePatternField.getSourceLocationRange()),
                name,
                expected
        );
    }

    /**
     * Visit a node pattern property node
     *
     * @param nodePatternProperty The base NodePatternProperty node from langkit
     * @return The NodePatternProperty node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NodePatternProperty nodePatternProperty) {
        // Translate the node pattern detail fields
        String propertyName = nodePatternProperty.fCall().fName().getText();
        ArgList argList = (ArgList) nodePatternProperty.fCall().fArguments().accept(this);
        DetailValue expected = (DetailValue) nodePatternProperty.fExpectedValue().accept(this);

        // Return the new node pattern property detail
        return NodePatternPropertyNodeGen.create(
                new SourceLocation(this.source, nodePatternProperty.getSourceLocationRange()),
                propertyName,
                argList,
                expected
        );
    }

    /**
     * Visit a node pattern selector node
     *
     * @param nodePatternSelector The base NodePatternSelector node from langkit
     * @return The NodePatternSelector node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.NodePatternSelector nodePatternSelector) {
        // Translate the selector call
        SelectorCall selectorCall = (SelectorCall) nodePatternSelector.fCall().accept(this);

        // Open a lexical scope
        this.scope.openLexical();

        // Translate the pattern
        BasePattern pattern = (BasePattern) nodePatternSelector.fPattern().accept(this);

        // Close the scope
        this.scope.closeLexical();

        // Return the new selector node pattern detail
        return new NodePatternSelector(
                new SourceLocation(this.source, nodePatternSelector.getSourceLocationRange()),
                selectorCall,
                pattern
        );
    }


    // --- Chained pattern

    /**
     * Visit a chained node pattern node
     *
     * @param chainedNodePattern The base ChainedNodePattern node from langkit
     * @return The ChainedNodePattern node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.ChainedNodePattern chainedNodePattern) {
        // Translate the chained node pattern fields
        BasePattern nodePattern = (BasePattern) chainedNodePattern.fFirstPattern().accept(this);

        // Get the chained pattern link list
        List<ChainedPatternLink> chain = new ArrayList<>();
        try(Liblkqllang.LkqlNodeArray array = chainedNodePattern.fChain().children()) {
            for(Liblkqllang.LkqlNode node : array) {
                chain.add((ChainedPatternLink) node.accept(this));
            }
        }

        // Return the new chained node pattern node
        return new ChainedNodePattern(
                new SourceLocation(this.source, chainedNodePattern.getSourceLocationRange()),
                nodePattern,
                chain.toArray(new ChainedPatternLink[0])
        );
    }

    /**
     * Visit a field link node
     *
     * @param fieldLink The base FieldLink node from langkit
     * @return The FieldLink node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.FieldLink fieldLink) {
        // Translate the field link fields
        BasePattern pattern = (BasePattern) fieldLink.fPattern().accept(this);
        String fieldName = fieldLink.fField().getText();

        // Return the new field link node
        return FieldLinkNodeGen.create(
                new SourceLocation(this.source, fieldLink.getSourceLocationRange()),
                pattern,
                fieldName
        );
    }

    /**
     * Visit a property link node
     *
     * @param propertyLink The base PropertyLink node from langkit
     * @return The PropertyLink node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.PropertyLink propertyLink) {
        // Translate the property link fields
        BasePattern pattern = (BasePattern) propertyLink.fPattern().accept(this);
        String propertyName = propertyLink.fProperty().fName().getText();
        ArgList argList = (ArgList) propertyLink.fProperty().fArguments().accept(this);

        // Return the new field link node
        return PropertyLinkNodeGen.create(
                new SourceLocation(this.source, propertyLink.getSourceLocationRange()),
                pattern,
                propertyName,
                argList
        );
    }

    /**
     * Visit a selector link node
     *
     * @param selectorLink The base SelectorLink node from langkit
     * @return The SelectorLink node for Truffle
     */
    @Override
    public LKQLNode visit(Liblkqllang.SelectorLink selectorLink) {
        // Translate the selector call
        SelectorCall selectorCall = (SelectorCall) selectorLink.fSelector().accept(this);

        // Open a lexical scope
        this.scope.openLexical();

        // Translate the pattern
        BasePattern pattern = (BasePattern) selectorLink.fPattern().accept(this);

        // Close the scope
        this.scope.closeLexical();

        // Return the new selector link node
        return new SelectorLink(
                new SourceLocation(this.source, selectorLink.getSourceLocationRange()),
                pattern,
                selectorCall
        );
    }


    // --- Unvisited nodes

    // Operators

    @Override
    public LKQLNode visit(Liblkqllang.OpPlus opPlus) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpMinus opMinus) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpMul opMul) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpDiv opDiv) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpAnd opAnd) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpOr opOr) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpEq opEq) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpNeq opNeq) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpConcat opConcat) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpLt opLt) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpLeq opLeq) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpGt opGt) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpGeq opGeq) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.OpNot opNot) { return null; }

    // Helper nodes

    @Override
    public LKQLNode visit(Liblkqllang.ExprList exprList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.ParameterDeclList parameterDeclList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.BlockBodyList blockBodyList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SubBlockLiteral subBlockLiteral) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SubBlockLiteralList subBlockLiteralList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.MatchArmList matchArmList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorArmList selectorArmList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorExprList selectorExprList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.NodePatternDetailList nodePatternDetailList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.ChainedPatternLinkList chainedPatternLinkList) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SafeAbsent safeAbsent) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SafePresent safePresent) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.QueryKindAll queryKindAll) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.QueryKindFirst queryKindFirst) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorExprModeDefault selectorExprModeDefault) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorExprModeRec selectorExprModeRec) { return null; }

    @Override
    public LKQLNode visit(Liblkqllang.SelectorExprModeSkip selectorExprModeSkip) { return null; }

}
