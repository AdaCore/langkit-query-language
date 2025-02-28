//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a boolean literal in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class BooleanLiteral extends Expr {

    // ----- Attributes -----

    /** The value of the boolean. */
    private final boolean value;

    // ----- Constructors -----

    /**
     * Create a new boolean literal with its value.
     *
     * @param location The location of the node in the source.
     * @param value The value of the boolean.
     */
    public BooleanLiteral(SourceSection location, boolean value) {
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
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeBoolean(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public boolean executeBoolean(VirtualFrame frame) {
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
