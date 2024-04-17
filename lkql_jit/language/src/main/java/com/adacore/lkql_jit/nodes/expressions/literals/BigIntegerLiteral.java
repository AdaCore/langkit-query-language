//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.math.BigInteger;

/**
 * This node represents a literal integer that cannot be contained in a java long.
 *
 * @author Hugo GUERRIER
 */
public final class BigIntegerLiteral extends Expr {

    // ----- Attributes -----

    /** The big integer value. */
    private final BigInteger value;

    // ----- Constructors -----

    /**
     * Create a new big integer literal with its value.
     *
     * @param location The location of the node in the source.
     * @param value The value of the big integer.
     */
    public BigIntegerLiteral(SourceLocation location, BigInteger value) {
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
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeBigInteger(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public BigInteger executeBigInteger(VirtualFrame frame) {
        return this.value;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"value"}, new Object[] {this.value});
    }
}
