/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.runtime.built_ins;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.runtime.values.FunctionValue;


/**
 * This class represents the base of a built-in function value
 *
 * @author Hugo GUERRIER
 */
public final class BuiltInFunctionValue extends FunctionValue {

    // ----- Attributes -----

    /**
     * The value of the "this" variable
     */
    private Object thisValue;

    // ----- Constructor -----

    /**
     * Create a built-in function value
     *
     * @param name          The name of the built-in
     * @param documentation The documentation of the built-in
     * @param names         The names of the built-in parameters
     * @param defaultValues The default values of the parameters
     * @param body          The expression representing the built-in body
     */
    public BuiltInFunctionValue(
        String name,
        String documentation,
        String[] names,
        Expr[] defaultValues,
        BuiltInExpr body
    ) {
        super(null, null, false, name, documentation, new int[0], names, defaultValues, body);
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
     * Set the calling node to the function body to allow its access in the built-in expression
     *
     * @param callNode The node which called the built-in
     */
    public void setCallNode(FunCall callNode) {
        ((BuiltInExpr) this.getBody()).setCallNode(callNode);
    }

}