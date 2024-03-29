//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This node represents the conditional branching expression in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class IfThenElse extends Expr {

    // ----- Children -----

    /** The condition of the branching. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr condition;

    /** The consequence of the branching. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr consequence;

    /** The alternative of the branching. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr alternative;

    // ----- Constructors -----

    /**
     * Create a new if then else node.
     *
     * @param location The location of the node in the source.
     * @param condition The condition expression.
     * @param consequence The consequence expression.
     * @param alternative The alternative expression.
     */
    public IfThenElse(SourceLocation location, Expr condition, Expr consequence, Expr alternative) {
        super(location);
        this.condition = condition;
        this.consequence = consequence;
        this.alternative = alternative;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Evaluate the condition as a boolean
        boolean conditionValue;
        try {
            conditionValue = this.condition.executeBoolean(frame);
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.condition);
        }

        // Execute the correct branching
        if (conditionValue) {
            return this.consequence.executeGeneric(frame);
        } else {
            return this.alternative.executeGeneric(frame);
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
