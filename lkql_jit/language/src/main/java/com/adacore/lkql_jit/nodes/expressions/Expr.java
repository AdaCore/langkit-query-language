//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.*;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.interfaces.*;
import com.adacore.lkql_jit.runtime.values.lists.LKQLLazyList;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/**
 * This node is the base of all expressions in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class Expr extends LKQLNode {

    // ----- Constructors -----

    /**
     * Create a new expression node.
     *
     * @param location The location of the node in the source.
     */
    protected Expr(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * Execute the expression as the unit value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as unit.
     * @throws UnexpectedResultException If the node cannot be evaluated as a unit value.
     */
    @SuppressWarnings("unused")
    public LKQLUnit executeUnit(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLUnit(executeGeneric(frame));
    }

    /**
     * Execute the expression as a nullish value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as nullish.
     * @throws UnexpectedResultException If the node cannot be evaluated as a nullish value.
     */
    @SuppressWarnings("unused")
    public Nullish executeNullish(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectNullish(executeGeneric(frame));
    }

    /**
     * Execute the expression as a boolean.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a boolean.
     * @throws UnexpectedResultException If the node cannot be evaluated as a boolean.
     */
    @SuppressWarnings("unused")
    public boolean executeBoolean(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectBoolean(executeGeneric(frame));
    }

    public Truthy executeTruthy(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectTruthy(executeGeneric(frame));
    }

    /**
     * Execute the expression as a long.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a long.
     * @throws UnexpectedResultException If the node cannot be evaluated as a long.
     */
    @SuppressWarnings("unused")
    public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLong(executeGeneric(frame));
    }

    /**
     * Execute the expression as a big integer.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a big integer.
     * @throws UnexpectedResultException If the node cannot be evaluated as a big integer.
     */
    @SuppressWarnings("unused")
    public BigInteger executeBigInteger(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectBigInteger(executeGeneric(frame));
    }

    /**
     * Execute the expression as a string.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a string.
     * @throws UnexpectedResultException If the node cannot be evaluated as a string.
     */
    @SuppressWarnings("unused")
    public String executeString(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectString(executeGeneric(frame));
    }

    /**
     * Execute the expression as a function value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a function value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a function.
     */
    @SuppressWarnings("unused")
    public LKQLFunction executeFunction(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLFunction(executeGeneric(frame));
    }

    /**
     * Execute the expression as a property reference value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a property reference value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a property reference.
     */
    @SuppressWarnings("unused")
    public LKQLProperty executeProperty(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLProperty(executeGeneric(frame));
    }

    /**
     * Execute the expression as a selector value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a selector value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a selector.
     */
    @SuppressWarnings("unused")
    public LKQLSelector executeSelector(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLSelector(executeGeneric(frame));
    }

    /**
     * Execute the expression as a tuple value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a tuple value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a tuple.
     */
    @SuppressWarnings("unused")
    public LKQLTuple executeTuple(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLTuple(executeGeneric(frame));
    }

    /**
     * Execute the expression as a list value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a list value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a list.
     */
    @SuppressWarnings("unused")
    public LKQLList executeList(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLList(executeGeneric(frame));
    }

    /**
     * Execute the expression as a lazy list value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a lazy list value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a lazy list.
     */
    @SuppressWarnings("unused")
    public LKQLLazyList executeLazyList(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLLazyList(executeGeneric(frame));
    }

    /**
     * Execute the expression as an indexable value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as an indexable value.
     * @throws UnexpectedResultException If the node cannot be evaluated as an indexable.
     */
    @SuppressWarnings("unused")
    public Indexable executeIndexable(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectIndexable(executeGeneric(frame));
    }

    /**
     * Execute the expression as an iterable value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as an iterable value.
     * @throws UnexpectedResultException If the node cannot be evaluated as an iterable.
     */
    @SuppressWarnings("unused")
    public Iterable executeIterable(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectIterable(executeGeneric(frame));
    }

    /**
     * Execute the expression as an object value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as an object value.
     * @throws UnexpectedResultException If the node cannot be evaluated as an object.
     */
    @SuppressWarnings("unused")
    public LKQLObject executeObject(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLObject(executeGeneric(frame));
    }

    /**
     * Execute the expression as a namespace value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a namespace value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a namespace.
     */
    @SuppressWarnings("unused")
    public LKQLNamespace executeNamespace(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLNamespace(executeGeneric(frame));
    }

    /**
     * Execute the expression as a LKQL value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a lkql value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a lkql value.
     */
    @SuppressWarnings("unused")
    public LKQLValue executeValue(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLValue(executeGeneric(frame));
    }

    /**
     * Execute the expression as a node.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as an node.
     * @throws UnexpectedResultException If the node cannot be evaluated as a node.
     */
    @SuppressWarnings("unused")
    public LangkitSupport.NodeInterface executeNode(VirtualFrame frame)
        throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectNodeInterface(executeGeneric(frame));
    }

    /**
     * Execute the expression as a rewriting context.
     *
     * @throws UnexpectedResultException If the node cannot be evaluated as a rewriting context.
     */
    @SuppressWarnings("unused")
    public Libadalang.RewritingContext executeRewritingContext(VirtualFrame frame)
        throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectRewritingContext(executeGeneric(frame));
    }

    /**
     * Execute the expression as a rewriting context.
     *
     * @throws UnexpectedResultException If the node cannot be evaluated as a rewriting context.
     */
    @SuppressWarnings("unused")
    public LangkitSupport.RewritingNodeInterface executeRewritingNode(VirtualFrame frame)
        throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectRewritingNodeInterface(executeGeneric(frame));
    }

    /**
     * Execute the expression as a member reference.
     *
     * @throws UnexpectedResultException If the node cannot be evaluated as a member reference.
     */
    @SuppressWarnings("unused")
    public Libadalang.MemberReference executeMemberReference(VirtualFrame frame)
        throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectMemberReference(executeGeneric(frame));
    }
}
