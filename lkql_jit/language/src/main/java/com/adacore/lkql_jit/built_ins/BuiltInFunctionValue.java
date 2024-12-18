//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;

/** This class represents the LKQL value of a built-in function. */
public class BuiltInFunctionValue extends LKQLFunction {

    // ----- Constructor -----

    /**
     * Create a built-in function value.
     *
     * @param name The name of the built-in.
     * @param documentation The documentation of the built-in.
     * @param names The names of the built-in parameters.
     * @param defaultValues The default values of the parameters.
     * @param body The expression representing the built-in body.
     */
    public BuiltInFunctionValue(
            String name,
            String documentation,
            String[] names,
            Expr[] defaultValues,
            AbstractBuiltInFunctionBody body) {
        super(
                new FunctionRootNode(null, null, false, body),
                Closure.EMPTY,
                name,
                documentation,
                names,
                defaultValues);
    }

    public BuiltInFunctionValue(
            String name,
            String documentation,
            String[] names,
            Expr[] defaultValues,
            AbstractBuiltInFunctionBody.BuiltInCallback fn) {
        super(
                new FunctionRootNode(
                        null, null, false, AbstractBuiltInFunctionBody.fromCallback(fn)),
                Closure.EMPTY,
                name,
                documentation,
                names,
                defaultValues);
    }

    // ----- Instance methods -----

    /**
     * Set the calling node to the function body to allow its access in the built-in expression.
     *
     * @param callNode The node which called the built-in.
     */
    public void setCallNode(FunCall callNode) {
        ((AbstractBuiltInFunctionBody) this.getBody()).setCallNode(callNode);
    }
}
