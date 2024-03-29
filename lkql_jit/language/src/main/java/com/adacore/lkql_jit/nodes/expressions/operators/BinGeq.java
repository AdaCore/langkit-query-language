//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import java.math.BigInteger;

/**
 * This node represents the "greater or equals" comparison in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinGeq extends BinOp {

    // ----- Constructors -----

    /**
     * Create a "greater or equals" node.
     *
     * @param location The location of the node in the source.
     * @param leftLocation The location of the left node.
     * @param rightLocation The location of the right node.
     */
    protected BinGeq(
            SourceLocation location, DummyLocation leftLocation, DummyLocation rightLocation) {
        super(location, leftLocation, rightLocation);
    }

    // ----- Execution methods -----

    /**
     * Do the "greater or equals" comparison on longs.
     *
     * @param left The left long value.
     * @param right The right long value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean geqLongs(long left, long right) {
        return left >= right;
    }

    /**
     * Do the "greater or equals" comparison on big integers.
     *
     * @param left The left big integer value.
     * @param right The right big integer value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean geqBigInteger(BigInteger left, BigInteger right) {
        return BigIntegerUtils.compareTo(left, right) >= 0;
    }

    /**
     * Do the "greater or equals" comparison on strings.
     *
     * @param left The left string value.
     * @param right The right string value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean geqString(String left, String right) {
        return left.compareTo(right) >= 0;
    }

    /**
     * Do the "greater of equals" comparison on not comparable values.
     *
     * @param left The left value.
     * @param right The right value.
     */
    @Fallback
    protected void notComparable(Object left, Object right) {
        throw LKQLRuntimeException.unsupportedOperation(
                LKQLTypesHelper.fromJava(left), ">=", LKQLTypesHelper.fromJava(right), this);
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
