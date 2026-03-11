//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.exceptions.LKQLStaticErrors;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.ExprArg;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverter;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverterNodeGen;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a constructor call in the LKQL language. For example new IntLiteral("42").
 */
public final class ConstructorCall extends Expr {

    // ----- Children -----

    /**
     * Arguments for the constructor calling. Those are only ExprArg because named arguments are
     * resolved when the Truffle node is created.
     */
    @Children
    private ExprArg[] args;

    /** Node for the conversions to rewriting node. */
    @Child
    private RewritingNodeConverter rewritingNodeConverter = RewritingNodeConverterNodeGen.create();

    // ----- Attributes -----

    /** Kind of the node to create. */
    public final LangkitSupport.NodeKindInterface nodeKind;

    /** Whether the created node is a token node. */
    public final boolean isTokenNode;

    /** Whether the created node is a list node. */
    public final boolean isListNode;

    // ----- Constructors -----

    /**
     * Create a new constructor call node.
     *
     * @param location The location of the node in the source.
     * @param kindName Kind of the node to create.
     * @param argList List of arguments for the constructor call.
     */
    public ConstructorCall(
        SourceSection location,
        Identifier kindName,
        Libadalang.Reflection.Node nodeDescription,
        ArgList argList,
        LKQLStaticErrors errors
    ) {
        super(location);
        this.nodeKind = nodeDescription.kind;
        this.isTokenNode = nodeDescription.isTokenNode;
        this.isListNode = nodeDescription.isListNode;

        // Check if the kind is concrete
        if (nodeKind == null) {
            errors.expectConcreteKind(kindName.getSourceSection());
            return;
        }

        // Create the argument expression array regarding the constructed node kind
        final var fieldIndexes = ArrayUtils.indexMap(nodeDescription.fields);
        if (this.isTokenNode) {
            this.args = new ExprArg[1];
        } else if (this.isListNode) {
            this.args = new ExprArg[Math.min(1, argList.getArgs().length)];
        } else {
            this.args = new ExprArg[nodeDescription.fields.length];
        }

        // Check the argument arity
        if (
            (this.args.length != argList.getArgs().length) &&
            (!this.isListNode || argList.getArgs().length > 1)
        ) {
            errors.wrongArity(this.args.length, argList.getArgs().length, location);
            return;
        }

        for (int i = 0; i < argList.getArgs().length; i++) {
            final var arg = argList.getArgs()[i];

            // If the argument is an expression arg, just add it to the children
            if (arg instanceof ExprArg exprArg) {
                this.args[i] = exprArg;
            }
            // Else, this is a named arg, so turn it into an expression arg and add it to children
            // at the right index.
            else {
                final var namedArg = (NamedArg) arg;
                final var name = namedArg.getArgStringName();

                // Check that the argument name is a valid field and get its index
                var fieldIndex = fieldIndexes.get(name);
                if (fieldIndex == null) {
                    errors.unknownArg(name, namedArg.getSourceSection());
                } else {
                    // Check that the child argument s empty and set it
                    var index = fieldIndex.getFirst();
                    if (this.args[index] != null) {
                        errors.namedOverlapPositional(namedArg.getSourceSection());
                    } else {
                        this.args[index] = new ExprArg(
                            namedArg.getSourceSection(),
                            namedArg.getArgExpr()
                        );
                    }
                }
            }
        }
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeRewritingNode(frame);
    }

    /**
     * @see Expr#executeRewritingNode(VirtualFrame)
     */
    @Override
    @ExplodeLoop
    public LangkitSupport.RewritingNodeInterface executeRewritingNode(VirtualFrame frame) {
        // Get the rewriting context from the language global context
        final var rewritingContext = LKQLLanguage.getContext(this).getRewritingContext();

        // If the created node is a token one, evaluate the sole argument as a string then call the
        // creation method.
        if (this.isTokenNode) {
            try {
                return rewritingContext.createTokenNode(
                    this.nodeKind,
                    this.args[0].getArgExpr().executeString(frame)
                );
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeError.wrongType(
                    LKQLTypesHelper.LKQL_STRING,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.args[0]
                );
            }
        }
        // Else, if the node is a list node, evaluate the first argument as a list
        else if (this.isListNode) {
            if (this.args.length == 0) {
                return rewritingContext.createNode(
                    this.nodeKind,
                    new LangkitSupport.RewritingNodeInterface[0]
                );
            } else {
                try {
                    final var objectArgs = this.args[0].getArgExpr()
                        .executeList(frame)
                        .getContent();
                    final var args = new LangkitSupport.RewritingNodeInterface[objectArgs.length];
                    for (int i = 0; i < objectArgs.length; i++) {
                        args[i] = this.rewritingNodeConverter.execute(
                            objectArgs[i],
                            true,
                            this.args[0].getArgExpr()
                        );
                    }
                    return rewritingContext.createNode(this.nodeKind, args);
                } catch (UnexpectedResultException e) {
                    throw LKQLRuntimeError.wrongType(
                        LKQLTypesHelper.LKQL_LIST,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.args[0].getArgExpr()
                    );
                }
            }
        }
        // Else, evaluate all arguments as rewriting nodes and call the constructor
        else {
            final var args = new LangkitSupport.RewritingNodeInterface[this.args.length];
            for (int i = 0; i < args.length; i++) {
                args[i] = this.rewritingNodeConverter.execute(
                    this.args[i].getArgExpr().executeGeneric(frame),
                    true,
                    this.args[i].getArgExpr()
                );
            }
            return rewritingContext.createNode(this.nodeKind, args);
        }
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(
            indentLevel,
            new String[] { "nodeKind" },
            new Object[] { this.nodeKind }
        );
    }
}
