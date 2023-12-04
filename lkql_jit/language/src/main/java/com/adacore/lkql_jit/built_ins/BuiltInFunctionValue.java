/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.built_ins.values.LKQLFunction;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.oracle.truffle.api.frame.VirtualFrame;

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
}
