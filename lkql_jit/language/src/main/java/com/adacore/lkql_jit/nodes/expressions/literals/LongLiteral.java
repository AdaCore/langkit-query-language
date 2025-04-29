//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a literal integer that fit in a long java value.
 *
 * @author Hugo GUERRIER
 */
public final class LongLiteral extends Expr {

    // ----- Attributes -----

    /** The value of the integer. */
    private final long value;

    // ----- Constructors -----

    /**
     * Create a new long literal with its value.
     *
     * @param location The location of the node in the source.
     * @param value The integer value.
     */
    public LongLiteral(SourceSection location, long value) {
        super(location);
        this.value = value;
    }

    // ----- Execute methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.value;
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeLong(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public long executeLong(VirtualFrame frame) {
        return this.value;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "value" },
                new Object[] { this.value }
            );
    }
}
