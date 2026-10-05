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

/** This node represents an arithmetic unary negation operation. */
public abstract class UnMinus extends UnOp {

    // ----- Constructors -----

    /**
     * Create an arithmetic unary negation node.
     *
     * @param location The location of the node in the source.
     */
    protected UnMinus(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /** Arithmetically negate the long value. */
    @Specialization
    protected long negateLong(long arg) {
        return -arg;
    }

    /** Arithmetically negate the big integer value. */
    @Specialization
    protected BigInteger negateBigInteger(BigInteger arg) {
        return BigIntegerUtils.negate(arg);
    }

    /** Fallback error method when the argument is a non-integer value. */
    @Fallback
    protected void notNumber(Object arg) {
        throw LKQLRuntimeError.wrongType(
            LKQLTypesHelper.LKQL_INTEGER,
            LKQLTypesHelper.fromJava(arg),
            this.getArg()
        );
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
