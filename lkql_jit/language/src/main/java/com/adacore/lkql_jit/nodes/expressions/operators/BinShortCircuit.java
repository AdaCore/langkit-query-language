//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This class represent the base of the binary operation node that are edibles to the short circuit
 * strategy.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinShortCircuit extends Expr {

    // ----- Children -----

    /** The left operand expression. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr left;

    /** The right operand expression. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr right;

    // ----- Constructors -----

    /**
     * Create a new bin op short circuit node.
     *
     * @param location The location of the node in the source.
     * @param left The left expression.
     * @param right The right expression.
     */
    protected BinShortCircuit(SourceSection location, Expr left, Expr right) {
        super(location);
        this.left = left;
        this.right = right;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeBoolean(frame);
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeBoolean(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public boolean executeBoolean(VirtualFrame frame) {
        // Execute the left value
        boolean leftValue;
        try {
            leftValue = this.left.executeBoolean(frame);
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.left);
        }

        // Execute the right value if needed
        boolean rightValue = false;
        if (this.doRightEvaluation(leftValue)) {
            try {
                rightValue = this.right.executeBoolean(frame);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_BOOLEAN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.right);
            }
        }

        // Return the result
        return this.execute(leftValue, rightValue);
    }

    // ----- Class methods -----

    /**
     * Get if the right execution is necessary.
     *
     * @param leftValue The left value.
     * @return True if the right execution is necessary, false else.
     */
    protected abstract boolean doRightEvaluation(boolean leftValue);

    /**
     * Do the execution for the left and right value.
     *
     * @param leftValue The left value.
     * @param rightValue The right value.
     * @return The result of the node execution.
     */
    protected abstract boolean execute(boolean leftValue, boolean rightValue);
}
