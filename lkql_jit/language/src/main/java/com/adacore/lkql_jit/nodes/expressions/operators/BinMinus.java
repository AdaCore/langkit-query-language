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

/** This node represents a subtraction operation. */
public abstract class BinMinus extends BinOp {

    // ----- Constructors -----

    /**
     * Create a binary subtraction node.
     *
     * @param location The location of the node in the source.
     */
    protected BinMinus(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /** Subtract two longs. */
    @Specialization(rewriteOn = ArithmeticException.class)
    protected long subLongs(long left, long right) {
        return Math.subtractExact(left, right);
    }

    /** Subtract two big integers. */
    @Specialization(replaces = "subLongs")
    protected BigInteger subBigIntegers(BigInteger left, BigInteger right) {
        return BigIntegerUtils.subtract(left, right);
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
