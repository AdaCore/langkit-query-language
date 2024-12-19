//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a list literal node in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ListLiteral extends Expr {

    // ----- Children -----

    /** The list expressions. */
    @Children private final Expr[] exprs;

    // ----- Constructors -----

    /**
     * Create a new list literal node.
     *
     * @param location The location of the node in the source.
     * @param exprs The expressions inside the list.
     */
    public ListLiteral(final SourceSection location, final Expr[] exprs) {
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
        return this.executeList(frame);
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeList(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    @ExplodeLoop
    public LKQLList executeList(VirtualFrame frame) {
        // Evaluate the list content
        Object[] values = new Object[this.exprs.length];
        for (int i = 0; i < this.exprs.length; i++) {
            values[i] = this.exprs[i].executeGeneric(frame);
        }

        // Return the list value
        return new LKQLList(values);
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
