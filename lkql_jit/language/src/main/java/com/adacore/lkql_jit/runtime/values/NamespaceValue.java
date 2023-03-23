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

import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.util_functions.ArrayUtils;
import com.oracle.truffle.api.CompilerDirectives;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;


/**
 * This class represents the namespace values in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class NamespaceValue implements LKQLValue {

    // ----- Attributes -----

    /** An array containing the name of the values in the namespace */
    private final String[] names;

    /** An array containing all values in the namespace */
    private final Object[] values;

    /** The node checkers that are accessible from the namespace */
    private final Map<String, ObjectValue> nodeCheckers;

    /** The unit checker that are accessible from the namespace */
    private final Map<String, ObjectValue> unitCheckers;

    // ----- Constructors -----

    /**
     * Create a new namespace with its name and bindings
     *
     * @param names The names of the variables to put in the namespace
     * @param values The values of the variables to put in the namespace
     */
    public NamespaceValue(
            String[] names,
            Object[] values,
            Map<String, ObjectValue> nodeCheckers,
            Map<String, ObjectValue> unitCheckers
    ) {
        this.names = names;
        this.values = values;
        this.nodeCheckers = nodeCheckers;
        this.unitCheckers = unitCheckers;
    }

    // ----- Getters -----

    public Object[] getValues() {
        return values;
    }

    public String[] getNames() {
        return names;
    }

    public Map<String, ObjectValue> getNodeCheckers() {
        return nodeCheckers;
    }

    public Map<String, ObjectValue> getUnitCheckers() {
        return unitCheckers;
    }

    /**
     * Get a value from the namespace with its name
     *
     * @param symbol The name of the value to get
     * @return The value if it exists, null else
     */
    public Object get(String symbol) {
        for(int i = 0 ; i < this.names.length ; i++) {
            if(this.names[i].equals(symbol)) return this.values[i];
        }
        return null;
    }

    // ----- Value methods -----

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue) */
    @Override
    public boolean internalEquals(LKQLValue o) {
        if(o == this) return true;
        if(!(o instanceof NamespaceValue other)) return false;
        return ArrayUtils.equals(this.values, other.values) &&
                ArrayUtils.equals(this.names, other.names);
    }
    
    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        // Create the string for the mapping
        StringBuilder symbols = new StringBuilder();
        for(int i = 0 ; i < this.names.length ; i++) {
            symbols.append(this.names[i]).append(": ").append(this.values[i].toString());
            if(i < this.names.length - 1) symbols.append(", ");
        }

        // Return the string
        return "Namespace <symbols = " + symbols +
                ", nodeCheckers = " + this.nodeCheckers +
                ", unitCheckers = " + this.unitCheckers + ">";
    }

}
