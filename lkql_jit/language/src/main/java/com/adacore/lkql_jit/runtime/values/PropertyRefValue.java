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

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.utils.UnsupportedTypeException;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.Locatable;
import com.adacore.lkql_jit.utils.util_functions.ReflectionUtils;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.adacore.libadalang.Libadalang;

import java.lang.reflect.Method;
import java.util.Map;


/**
 * This class represents a reference to an ada node property stored in a value
 *
 * @author Hugo GUERRIER
 */
public final class PropertyRefValue implements LKQLValue {

    // ----- Attributes -----

    /** The node to execute the property on */
    private final Libadalang.AdaNode node;

    /** The property name */
    private final String propertyName;

    /** The methods map */
    private final Map<Integer, Method> methods;

    // ----- Constructors -----

    /**
     * Create a new property reference value
     *
     * @param node The node to create the property from
     * @param propertyName The name of the property to get
     */
    public PropertyRefValue(
            Libadalang.AdaNode node,
            String propertyName
    ) {
        this.node = node;
        this.propertyName = propertyName;
        this.methods = node.getMethods(this.propertyName);
    }

    /**
     * Creator for the property reference value for the Truffle DSL
     *
     * @param node The node to create the property on
     * @param propertyName The name of the property to get
     * @return The property reference
     */
    public static PropertyRefValue create(
            Libadalang.AdaNode node,
            String propertyName
    ) {
        return new PropertyRefValue(node, propertyName);
    }

    // ----- Getters -----

    public Libadalang.AdaNode getNode() {
        return this.node;
    }

    public String getPropertyName() {
        return this.propertyName;
    }

    public Map<Integer, Method> getMethods() {
        return this.methods;
    }

    // ----- Class methods -----

    /**
     * Get if the property reference point to a node field
     *
     * @return True of the property is a field, false else
     */
    public boolean isField() {
        return this.propertyName.startsWith("f");
    }

    /**
     * Execute the property with the given arguments
     *
     * @param caller The locatable which called the execution
     * @param arguments The argument for the property call
     * @return The result of the property execution
     */
    public Object execute(Locatable caller, ArgList argList, Object ... arguments) {
        try {
            return LKQLTypesHelper.toLKQLValue(ReflectionUtils.callProperty(
                    this.node,
                    this.methods,
                    caller,
                    argList,
                    arguments
            ));
        } catch (UnsupportedTypeException e) {
            throw LKQLRuntimeException.unsupportedType(
                    e.getType(),
                    caller
            );
        }
    }

    /**
     * Execute the property as a field access without arguments
     *
     * @param caller The locatable which called the execution
     * @return The result of the field call
     */
    public Object executeAsField(Locatable caller) {
        try {
            return LKQLTypesHelper.toLKQLValue(ReflectionUtils.callProperty(
                    this.node,
                    this.methods,
                    caller,
                    null
            ));
        } catch (UnsupportedTypeException e) {
            throw LKQLRuntimeException.unsupportedType(
                    e.getType(),
                    caller
            );
        }
    }

    // ----- Value methods -----

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue) */
    @Override
    public boolean internalEquals(LKQLValue o) {
        if(o == this) return true;
        if(!(o instanceof PropertyRefValue other)) return false;
        return this.methods == other.methods;
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return "<PropertyRef " + this.node.toString() + StringUtils.toSnakeCase(this.propertyName) + ">";
    }

}
