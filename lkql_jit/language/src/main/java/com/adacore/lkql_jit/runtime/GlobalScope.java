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

package com.adacore.lkql_jit.runtime;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.utils.checkers.BaseChecker;
import com.oracle.truffle.api.CompilerDirectives;
import java.util.HashMap;
import java.util.Map;

/**
 * This class represents a global LKQL scope which is common to all LKQL scripts. This is shared
 * between all scripts during an execution.
 *
 * @author Hugo GUERRIER
 */
public final class GlobalScope {

    // ----- Attributes -----

    /** The defined LKQL rules. */
    private final Map<String, BaseChecker> checkers;

    /** The array containing the built-in functions and selectors. */
    private final Object[] builtIns;

    /** The meta tables that contains built-in methods. */
    private final Map<String, Map<String, BuiltInFunctionValue>> metaTables;

    /**
     * The global objects table. This is only used in the interactive interpreter, because in every
     * other context we have pre-computed frames and namespaces.
     */
    private final Map<String, Object> globalObjects;

    // ----- Constructors -----

    /**
     * Create a new global scope.
     *
     * @param buildInNb The number of built-in functions.
     */
    public GlobalScope(int buildInNb) {
        this.checkers = new HashMap<>();
        this.builtIns = new Object[buildInNb];
        this.metaTables = new HashMap<>();
        this.globalObjects = new HashMap<>();
    }

    // ----- Instance methods -----

    /**
     * Get the LKQL checkers.
     *
     * @return The LKQL checkers.
     */
    public Map<String, BaseChecker> getCheckers() {
        return this.checkers;
    }

    /**
     * Add the given LKQL checker in the global values.
     *
     * @param name The name of the checker.
     * @param checker The object representing the checker.
     */
    @CompilerDirectives.TruffleBoundary
    public void addChecker(String name, BaseChecker checker) {
        this.checkers.put(name, checker);
    }

    /**
     * Get the built-in value at the given slot.
     *
     * @param slot The slot to get the built-in at.
     * @return The built-in value.
     */
    public Object getBuiltIn(int slot) {
        return this.builtIns[slot];
    }

    /**
     * Set a built-in value, this function is only used in built-in factory.
     *
     * @param slot The slot to set the built-in in.
     * @param value The value of the built-in to set.
     */
    public void setBuiltIn(int slot, Object value) {
        this.builtIns[slot] = value;
    }

    /**
     * Get a meta table for the given type.
     *
     * @param type The type to get the meta table for.
     * @return The meta table.
     */
    @CompilerDirectives.TruffleBoundary
    public Map<String, BuiltInFunctionValue> getMetaTable(String type) {
        return this.metaTables.get(type);
    }

    /**
     * Put a new meta table for a given type.
     *
     * @param type The type of the meta table.
     * @param methods The methods for the type.
     */
    @CompilerDirectives.TruffleBoundary
    public void putMetaTable(String type, Map<String, BuiltInFunctionValue> methods) {
        this.metaTables.put(type, methods);
    }

    public Map<String, Object> getGlobalObjects() {
        return globalObjects;
    }
}
