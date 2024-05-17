//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.built_ins.values.LKQLTuple;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a tuple literal declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class TupleLiteral extends Expr {

    // ----- Children -----

    /** The expressions contained in the tuple. */
    @Children private final Expr[] exprs;

    // ----- Constructors -----

    /**
     * Create a new tuple literal node.
     *
     * @param location The location of the node in the source.
     * @param exprs The expressions that are inside the tuple.
     */
    public TupleLiteral(SourceSection location, Expr[] exprs) {
        super(location);
        this.exprs = exprs;
    }

    // ----- Execute methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeTuple(frame);
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeTuple(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    @ExplodeLoop
    public LKQLTuple executeTuple(VirtualFrame frame) {
        // Evaluate the tuple values
        Object[] values = new Object[this.exprs.length];
        for (int i = 0; i < this.exprs.length; i++) {
            values[i] = this.exprs[i].executeGeneric(frame);
        }

        // Return the tuple value
        return new LKQLTuple(values);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
