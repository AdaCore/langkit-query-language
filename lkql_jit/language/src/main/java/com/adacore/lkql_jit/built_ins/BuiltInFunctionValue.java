//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.built_ins.values.LKQLFunction;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.Map;

/**
 * This class represents the base of a built-in function value.
 *
 * @author Hugo GUERRIER
 */
public final class BuiltInFunctionValue extends LKQLFunction {

    // ----- Attributes -----

    /** The value of the "this" variable. */
    private Object thisValue;

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
            BuiltinFunctionBody body) {
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
            BuiltinFunctionCallback fn) {
        super(
                new FunctionRootNode(
                        null,
                        null,
                        false,
                        new BuiltinFunctionBody() {
                            @Override
                            public Object executeGeneric(VirtualFrame frame) {
                                return fn.apply(frame, this.callNode);
                            }
                        }),
                Closure.EMPTY,
                name,
                documentation,
                names,
                defaultValues);
    }

    // ----- Getters -----

    public Object getThisValue() {
        return this.thisValue;
    }

    // ----- Setters -----

    public void setThisValue(Object thisValue) {
        this.thisValue = thisValue;
    }

    // ----- Class methods -----

    /**
     * Set the calling node to the function body to allow its access in the built-in expression.
     *
     * @param callNode The node which called the built-in.
     */
    public void setCallNode(FunCall callNode) {
        ((BuiltinFunctionBody) this.getBody()).setCallNode(callNode);
    }

    /** Function interface for lambda constructor to {@link BuiltInFunctionValue}. */
    public interface BuiltinFunctionCallback {
        public Object apply(VirtualFrame frame, FunCall call);
    }

    public static Map.Entry<String, BuiltInFunctionValue> create(
            String name,
            String doc,
            String[] names,
            Expr[] defaultValues,
            BuiltinFunctionCallback callback) {
        return Map.entry(name, new BuiltInFunctionValue(name, doc, names, defaultValues, callback));
    }

    public static Map.Entry<String, BuiltInFunctionValue> create(
            String name,
            String doc,
            String[] names,
            Expr[] defaultValues,
            BuiltinFunctionBody body) {
        return Map.entry(name, new BuiltInFunctionValue(name, doc, names, defaultValues, body));
    }
}
