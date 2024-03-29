//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;

/**
 * This node represents a body of a built-in function.
 *
 * @author Hugo GUERRIER
 */
public abstract class BuiltinFunctionBody extends Expr {

    // ----- Attributes -----

    /** The node that called the expression. */
    protected FunCall callNode;

    // ----- Constructors -----

    /** Create a new expression for a built-in function. */
    protected BuiltinFunctionBody() {
        super(null);
    }

    // ----- Setters -----

    public void setCallNode(FunCall callNode) {
        this.callNode = callNode;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return "BUILT_IN NODE";
    }
}
