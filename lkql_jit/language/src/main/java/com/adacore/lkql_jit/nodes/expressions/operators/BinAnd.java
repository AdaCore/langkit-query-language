//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represent the logic "and" operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class BinAnd extends BinShortCircuit {

    // ----- Constructors -----

    /**
     * Create an "and" node.
     *
     * @param location The location of the node in the source.
     * @param left The left expression.
     * @param right The right expression.
     */
    public BinAnd(SourceSection location, Expr left, Expr right) {
        super(location, left, right);
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.operators.BinShortCircuit#doRightEvaluation(boolean)
     */
    @Override
    protected boolean doRightEvaluation(boolean leftValue) {
        return leftValue;
    }

    /**
     * @see com.adacore.lkql_jit.nodes.expressions.operators.BinShortCircuit#execute(boolean,
     *     boolean)
     */
    @Override
    protected boolean execute(boolean leftValue, boolean rightValue) {
        return leftValue && rightValue;
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
