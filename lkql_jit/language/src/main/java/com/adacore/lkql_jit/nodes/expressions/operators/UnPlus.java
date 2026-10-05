//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/** This node represents an arithmetic unary "plus" operation. */
public abstract class UnPlus extends UnOp {

    // ----- Constructors -----

    /**
     * Create an arithmetic unary "plus" node.
     *
     * @param location The location of the node in the source.
     */
    protected UnPlus(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /** Return the long value unchanged. */
    @Specialization
    protected long getLong(long arg) {
        return arg;
    }

    /** Return the big integer value unchanged. */
    @Specialization
    protected BigInteger getBigInteger(BigInteger arg) {
        return arg;
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
