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

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;


/**
 * This class represents a functional value in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public class FunctionValue implements LKQLValue {

    // ----- Attributes -----

    /**
     * The function root node.
     */
    private final FunctionRootNode rootNode;

    /**
     * The closure of the function.
     */
    private final Closure closure;

    /**
     * The name of the function.
     */
    private String name;

    /**
     * The string representing the function documentation.
     */
    private final String documentation;

    /**
     * The name of the parameters.
     */
    private final String[] paramNames;

    /**
     * The default values of the parameters.
     */
    private final Expr[] defaultValues;

    // ----- Constructors -----

    /**
     * Create a new function value.
     *
     * @param rootNode      The function root node.
     * @param closure       The closure of the function.
     * @param name          The name of the function.
     * @param documentation The documentation of the function.
     * @param paramNames    The names of the parameters.
     * @param values        The default values of the parameters.
     */
    public FunctionValue(
        FunctionRootNode rootNode,
        Closure closure,
        String name,
        String documentation,
        String[] paramNames,
        Expr[] values
    ) {
        this.rootNode = rootNode;
        this.closure = closure;
        this.name = name;
        this.documentation = documentation;
        this.paramNames = paramNames;
        this.defaultValues = values;
    }

    // ----- Getters -----

    public Closure getClosure() {
        return this.closure;
    }

    public String getName() {
        return this.name;
    }

    @Override
    public String getDocumentation() {
        return this.documentation;
    }

    public String[] getParamNames() {
        return this.paramNames;
    }

    public Expr[] getDefaultValues() {
        return this.defaultValues;
    }

    public Expr getBody() {
        return this.rootNode.getBody();
    }

    public CallTarget getCallTarget() {
        return this.rootNode.getRealCallTarget();
    }

    // ----- Setters -----

    public void setName(String name) {
        this.name = name;
    }

    public void setMemoized(boolean isMemoized) {
        this.rootNode.setMemoized(isMemoized);
    }

    // ----- Value methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean internalEquals(LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof FunctionValue other)) return false;
        return this.rootNode.equals(other.rootNode);
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return "function<" + this.name + ">";
    }

}
