//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.oracle.truffle.api.frame.VirtualFrame;

/** This node represents a body of a built-in executable value. */
public abstract class BuiltInBody extends Expr {

    // ----- Attributes -----

    /** The node that called the expression. */
    protected FunCall callNode;

    // ----- Constructors -----

    /** Create a new expression for a built-in function. */
    protected BuiltInBody() {
        super(null);
    }

    // ----- Setters -----

    public void setCallNode(FunCall callNode) {
        this.callNode = callNode;
    }

    // ----- Class methods -----

    /** Create a new built-in function body from the given callback representing its execution. */
    public static BuiltInBody fromCallback(BuiltInCallback callback) {
        return new BuiltInBody() {
            @Override
            public Object executeGeneric(VirtualFrame frame) {
                return callback.apply(frame, this.callNode);
            }
        };
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return "BUILT_IN NODE";
    }

    // ----- Inner classes -----

    /** Function interface for lambda constructor to {@link BuiltInFunctionValue}. */
    public interface BuiltInCallback {
        public Object apply(VirtualFrame frame, FunCall call);
    }
}
