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

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.ExprArg;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverter;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverterNodeGen;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This node represents a constructor call in the LKQL language. For example new IntLiteral("42").
 */
public final class ConstructorCall extends Expr {

    // ----- Children -----

    /**
     * Arguments for the constructor calling. Those are only ExprArg because named arguments are
     * resolved when the Truffle node is created.
     */
    @Children private ExprArg[] args;

    /** Node for the conversions to rewriting node. */
    @Child
    private RewritingNodeConverter rewritingNodeConverter = RewritingNodeConverterNodeGen.create();

    // ----- Attributes -----

    /** Kind of the node to create. */
    public final Libadalang.NodeKind nodeKind;

    /** Whether the created node is a token node. */
    public final boolean isTokenNode;

    // ----- Constructors -----

    /**
     * Create a new constructor call node.
     *
     * @param location The location of the node in the source.
     * @param kindName Kind of the node to create.
     * @param argList List of arguments for the constructor call.
     */
    public ConstructorCall(SourceLocation location, Identifier kindName, ArgList argList) {
        super(location);

        // Get the node kind and if this is a token node
        final var description = Libadalang.NODE_DESCRIPTION_MAP.get(kindName.getName());
        if (description == null) {
            throw LKQLRuntimeException.invalidKindName(kindName);
        } else if (description.kind == null) {
            throw LKQLRuntimeException.invalidAbstractKind(kindName);
        }
        this.nodeKind = description.kind;
        this.isTokenNode = description.isTokenNode;

        // Verify and get the constructor arguments
        final var fieldIndexes = ArrayUtils.indexMap(description.fields);
        if (this.isTokenNode) {
            this.args = new ExprArg[1];
        } else if (description.isListNode) {
            this.args = new ExprArg[argList.getArgs().length];
        } else {
            this.args = new ExprArg[description.fields.length];
        }

        // Check the argument arity
        if (this.args.length != argList.getArgs().length) {
            throw LKQLRuntimeException.wrongArity(this.args.length, argList.getArgs().length, this);
        }

        for (int i = 0; i < argList.getArgs().length; i++) {
            final var arg = argList.getArgs()[i];

            // If the argument is an expression arg, just add it to the children
            if (arg instanceof ExprArg exprArg) {
                this.args[i] = exprArg;
            }

            // Else, this is a named arg, so turn it into an expression arg and add it to children
            else {
                final var namedArg = (NamedArg) arg;
                final var name = namedArg.getArgName().getName();

                // Check that the argument name is a valid field and get its index
                if (!fieldIndexes.containsKey(name)) {
                    throw LKQLRuntimeException.unknownArgument(name, namedArg.getArgName());
                }
                final int index = fieldIndexes.get(name).get(0);

                // Check that the child argument is empty and set it
                if (this.args[index] != null) {
                    throw LKQLRuntimeException.namedOverlapPositional(namedArg);
                }
                this.args[index] = new ExprArg(namedArg.getLocation(), namedArg.getArgExpr());
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
    public Libadalang.RewritingNode executeRewritingNode(VirtualFrame frame) {
        // Get the rewriting context from the language global context
        final var rewritingContext = LKQLLanguage.getContext(this).getRewritingContext();

        // If the created node is a token one, evaluate the sole argument as a string then call the
        // creation method.
        if (this.isTokenNode) {
            try {
                return rewritingContext.createTokenNode(
                        this.nodeKind, this.args[0].getArgExpr().executeString(frame));
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.args[0]);
            }
        }

        // Else, evaluate all arguments as rewriting nodes and call the constructor.
        else {
            final var args = new Libadalang.RewritingNode[this.args.length];
            for (int i = 0; i < args.length; i++) {
                args[i] =
                        this.rewritingNodeConverter.execute(
                                this.args[i].getArgExpr().executeGeneric(frame),
                                this.args[i].getArgExpr());
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
                indentLevel, new String[] {"nodeKind"}, new Object[] {this.nodeKind});
    }
}
