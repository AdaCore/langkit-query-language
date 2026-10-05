//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/** This node represents a division operation. */
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

    /** Divide two longs (rounded to inferior). */
    @Specialization
    protected long divLongs(long left, long right) {
        if (right == 0L) {
            throw LKQLRuntimeError.divByZero(this);
        }
        return Math.floorDiv(left, right);
    }

    /** Divide two big integers (rounded to inferior). */
    @Specialization
    protected BigInteger divBigIntegers(BigInteger left, BigInteger right) {
        if (BigIntegerUtils.equals(right, BigInteger.ZERO)) {
            throw LKQLRuntimeError.divByZero(this);
        }
        return BigIntegerUtils.divide(left, right);
    }

    /** Raise a type exception if there is a non-integer operand. */
    @Fallback
    protected void notNumbers(Object left, Object right) {
        if (!LKQLTypeSystemGen.isLong(left) && !LKQLTypeSystemGen.isBigInteger(left)) {
            throw LKQLRuntimeError.wrongType(
                LKQLTypesHelper.LKQL_INTEGER,
                LKQLTypesHelper.fromJava(left),
                this
            );
        } else {
            throw LKQLRuntimeError.wrongType(
                LKQLTypesHelper.LKQL_INTEGER,
                LKQLTypesHelper.fromJava(right),
                this
            );
        }
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
