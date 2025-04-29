//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/**
 * This node represents the arithmetic unary "plus" operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
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

    /**
     * Return the positivated long value.
     *
     * @param arg The value to positivate.
     * @return The positivated value.
     */
    @Specialization
    protected long getLong(long arg) {
        return arg;
    }

    /**
     * Return the positivated big integer value.
     *
     * @param arg The value to positivate.
     * @return The positivated value.
     */
    @Specialization
    protected BigInteger getBigInteger(BigInteger arg) {
        return arg;
    }

    /**
     * Fallback error method when the argument is a non-integer value.
     *
     * @param arg The non-integer argument.
     */
    @Fallback
    protected void notNumber(Object arg) {
        throw LKQLRuntimeException.wrongType(
            LKQLTypesHelper.LKQL_INTEGER,
            LKQLTypesHelper.fromJava(arg),
            this.getArg()
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
