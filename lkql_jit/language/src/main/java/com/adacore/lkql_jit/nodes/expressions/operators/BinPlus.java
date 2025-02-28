//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/**
 * This node represents the addition operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinPlus extends BinOp {

    // ----- Constructors -----

    /**
     * Create a binary addition node.
     *
     * @param location The location of the node in the source.
     */
    protected BinPlus(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * Add two longs.
     *
     * @param left The left long value.
     * @param right The right long value.
     * @return The addition of two longs.
     */
    @Specialization(rewriteOn = ArithmeticException.class)
    protected long addLongs(long left, long right) {
        return Math.addExact(left, right);
    }

    /**
     * Add two big integers.
     *
     * @param left The left long value.
     * @param right The right long value.
     * @return The addition of two big integers.
     */
    @Specialization(replaces = "addLongs")
    protected BigInteger addBigIntegers(BigInteger left, BigInteger right) {
        return BigIntegerUtils.add(left, right);
    }

    /**
     * Raise a type exception if there is a non-integer parameter.
     *
     * @param left The left value.
     * @param right The right value.
     */
    @Fallback
    protected void notNumbers(Object left, Object right) {
        if (!LKQLTypeSystemGen.isLong(left) && !LKQLTypeSystemGen.isBigInteger(left)) {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_INTEGER,
                LKQLTypesHelper.fromJava(left),
                this.getLeft()
            );
        } else {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_INTEGER,
                LKQLTypesHelper.fromJava(right),
                this.getRight()
            );
        }
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
