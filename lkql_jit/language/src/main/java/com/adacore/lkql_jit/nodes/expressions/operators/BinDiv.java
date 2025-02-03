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
 * This node represents the "division" operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinDiv extends BinOp {

    // ----- Constructors -----

    /**
     * Create a division node.
     *
     * @param location The location of the node in the source.
     */
    protected BinDiv(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * Divide two longs.
     *
     * @param left The left long value.
     * @param right The right long value.
     * @return The division of two longs (round to inferior).
     */
    @Specialization
    protected long divLongs(long left, long right) {
        if (right == 0L) {
            throw LKQLRuntimeException.divByZero(this);
        }
        return Math.floorDiv(left, right);
    }

    /**
     * Divide two big integers.
     *
     * @param left The left big integer value.
     * @param right The right big integer value.
     * @return The division of two big integers (round to inferior).
     */
    @Specialization
    protected BigInteger divBigIntegers(BigInteger left, BigInteger right) {
        if (BigIntegerUtils.equals(right, BigInteger.ZERO)) {
            throw LKQLRuntimeException.divByZero(this);
        }
        return BigIntegerUtils.divide(left, right);
    }

    /**
     * Raise a type exception if there is a non-integer operand.
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
                this
            );
        } else {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_INTEGER,
                LKQLTypesHelper.fromJava(right),
                this
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
