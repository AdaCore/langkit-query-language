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

import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;


/**
 * This class represents a functional value in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public class FunctionValue implements LKQLValue {

    // ----- Attributes -----

    /**
     * The name of the function
     */
    @CompilerDirectives.CompilationFinal
    private String name;

    /**
     * The string representing the function documentation
     */
    private final String documentation;

    /**
     * The name of the parameters
     */
    private final String[] paramNames;

    /**
     * The default values of the parameters
     */
    private final Expr[] defaultValues;

    /**
     * The namespace of the function
     */
    private NamespaceValue namespace;

    /**
     * The function root node
     */
    private final FunctionRootNode rootNode;

    // ----- Constructors -----

    /**
     * Create a new function value in the LKQL context
     *
     * @param descriptor    The descriptor of the function frame
     * @param closure       The closure of the function
     * @param isMemoized    If the function is memoized
     * @param name          The name of the function (this can be null)
     * @param documentation The documentation of the function
     * @param slots         The slots for the function arguments
     * @param paramNames    The names of the parameters
     * @param values        The default values of the parameters
     * @param body          The body of the function
     */
    @CompilerDirectives.TruffleBoundary
    public FunctionValue(
        FrameDescriptor descriptor,
        Closure closure,
        boolean isMemoized,
        String name,
        String documentation,
        int[] slots,
        String[] paramNames,
        Expr[] values,
        Expr body
    ) {
        this.name = name;
        this.documentation = documentation;
        this.paramNames = paramNames;
        this.defaultValues = values;
        this.namespace = null;
        this.rootNode = new FunctionRootNode(
            LKQLLanguage.getLanguage(body),
            descriptor,
            closure,
            isMemoized,
            slots,
            name,
            body
        );
        LKQLContext context = LKQLLanguage.getContext(this.rootNode.getBody());
        if (context != null && context.getGlobalValues().getNamespaceStack().size() > 0) {
            this.namespace = context.getGlobalValues().getNamespaceStack().peek();
        }
    }

    // ----- Getters -----

    public String getName() {
        return this.name;
    }

    @Override
    public String getDocumentation() {
        return this.documentation;
    }

    public NamespaceValue getNamespace() {
        return this.namespace;
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

    public Closure getClosure() {
        return this.rootNode.getClosure();
    }

    public CallTarget getCallTarget() {
        return this.rootNode.getRealCallTarget();
    }

    public FunctionRootNode getRootNode() {
        return rootNode;
    }

    // ----- Setters -----

    public void setName(String name) {
        this.name = name;
        this.rootNode.setName(name);
    }

    public void setNamespace(NamespaceValue namespace) {
        this.namespace = namespace;
    }

    public void setMemoized(boolean isMemoized) {
        this.rootNode.setMemoized(isMemoized);
    }

    // ----- Value methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    public boolean internalEquals(LKQLValue o) {
        // TODO : Compare functions
        return o == this;
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return "function<" + this.name + ">";
    }

}
