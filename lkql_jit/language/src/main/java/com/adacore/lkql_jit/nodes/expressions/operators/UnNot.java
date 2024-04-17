//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.built_ins.values.interfaces.Truthy;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

/**
 * This node represents the logic unary negation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class UnNot extends UnOp {

    // ----- Constructors -----

    /**
     * Create a logic unary negation node.
     *
     * @param location The location of the node in the source.
     * @param argLocation The location of the argument node.
     */
    protected UnNot(SourceLocation location, DummyLocation argLocation) {
        super(location, argLocation);
    }

    // ----- Execution methods -----

    /**
     * Logically negate a boolean value.
     *
     * @param arg The value to negate.
     * @return The negated value.
     */
    @Specialization
    protected boolean negateBoolean(boolean arg) {
        return !arg;
    }

    /**
     * Logically negate the truthy value.
     *
     * @param arg The value to negate.
     * @return The negated value.
     */
    @Specialization
    protected boolean negateTruthy(Truthy arg) {
        return !arg.isTruthy();
    }

    /**
     * Fallback error method when the argument is a non-truthy value.
     *
     * @param arg The non-truthy argument.
     */
    @Fallback
    protected void notBoolean(Object arg) {
        throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_BOOLEAN, LKQLTypesHelper.fromJava(arg), this.argLocation);
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
