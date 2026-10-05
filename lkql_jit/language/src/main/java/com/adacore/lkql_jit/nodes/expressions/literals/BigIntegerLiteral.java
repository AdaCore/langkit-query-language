//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/** This node represents an integer literal that cannot be contained in a Java long. */
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
    public BigIntegerLiteral(SourceSection location, BigInteger value) {
        super(location);
        this.value = value;
    }

    // ----- Execute methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.value;
    }

    @Override
    public BigInteger executeBigInteger(VirtualFrame frame) {
        return this.value;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[] { "value" },
            new Object[] { this.value }
        );
    }
}
