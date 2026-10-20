//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/** This node represents a "greater or equals" operation. */
public abstract class BinGeq extends BinOp {

    // ----- Constructors -----

    /**
     * Create a "greater or equals" node.
     *
     * @param location The location of the node in the source.
     */
    protected BinGeq(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /** Do the "greater or equals" comparison on longs. */
    @Specialization
    protected boolean geqLongs(long left, long right) {
        return left >= right;
    }

    /** Do the "greater or equals" comparison on big integers. */
    @Specialization
    protected boolean geqBigInteger(BigInteger left, BigInteger right) {
        return BigIntegerUtils.compareTo(left, right) >= 0;
    }

    /** Do the "greater or equals" comparison on strings. */
    @Specialization
    protected boolean geqString(String left, String right) {
        return left.compareTo(right) >= 0;
    }

    /** Do the "greater of equals" comparison on not comparable values. */
    @Fallback
    protected void notComparable(Object left, Object right) {
        throw LKQLRuntimeError.unsupportedOperation(
            LKQLTypesHelper.fromJava(left),
            ">=",
            LKQLTypesHelper.fromJava(right),
            this
        );
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
