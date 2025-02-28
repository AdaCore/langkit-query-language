//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/**
 * This node represents the "grater than" operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinGt extends BinOp {

    // ----- Constructors -----

    /**
     * Create a "greater than" node.
     *
     * @param location The location of the node in the source.
     */
    protected BinGt(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * Do the "greater than" comparison on longs.
     *
     * @param left The left long value.
     * @param right The right long value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean gtLongs(long left, long right) {
        return left > right;
    }

    /**
     * Do the "greater than" comparison on big integers.
     *
     * @param left The left big integer value.
     * @param right The right big integer value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean gtBigInteger(BigInteger left, BigInteger right) {
        return BigIntegerUtils.compareTo(left, right) > 0;
    }

    /**
     * Do the "greater than" comparison on strings.
     *
     * @param left The left string value.
     * @param right The right string value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean gtString(String left, String right) {
        return left.compareTo(right) > 0;
    }

    /**
     * Do the "greater than" comparison on not comparable values.
     *
     * @param left The left value.
     * @param right The right value.
     */
    @Fallback
    protected void notComparable(Object left, Object right) {
        throw LKQLRuntimeException.unsupportedOperation(
            LKQLTypesHelper.fromJava(left),
            ">",
            LKQLTypesHelper.fromJava(right),
            this
        );
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
