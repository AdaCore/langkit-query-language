//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a logic implication expression in the LKQL language. */
public class BinImply extends BinShortCircuit {

    // ----- Constructors -----

    public BinImply(SourceSection location, Expr left, Expr right) {
        super(location, left, right);
    }

    // ----- Execution methods -----

    @Override
    protected boolean doRightEvaluation(boolean leftValue) {
        return leftValue;
    }

    @Override
    protected boolean execute(boolean leftValue, boolean rightValue) {
        return !leftValue || rightValue;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
