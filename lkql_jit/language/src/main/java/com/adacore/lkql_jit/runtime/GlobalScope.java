//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.built_ins.BuiltInsHolder;
import com.adacore.lkql_jit.checker.BaseChecker;
import com.adacore.lkql_jit.runtime.values.LKQLNamespace;
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

    /** The array containing the built-in functions . */
    private final Object[] builtIns;

    public LKQLNamespace prelude = null;

    public HashMap<String, Integer> preludeMap = new HashMap<>();
    public Object[] preludeObjects = null;

    /** The meta tables that contains built-in methods. */
    private final Map<String, Map<String, BuiltInMethodFactory>> metaTables;

    /**
     * The global objects table. This is only used in the interactive interpreter, because in every
     * other context we have pre-computed frames and namespaces.
     */
    private final Map<String, Object> globalObjects;

    // ----- Constructors -----

    /** Create a new global scope. */
    public GlobalScope() {
        var builtInsHolder = BuiltInsHolder.get();
        this.checkers = new HashMap<>();
        this.builtIns =
                new Object
                        [builtInsHolder.builtInFunctions.size()
                                + builtInsHolder.builtInMethods.size()];
        this.metaTables = new HashMap<>();
        this.globalObjects = new HashMap<>();

        // Add the built-in functions
        for (int i = 0; i < builtInsHolder.builtInFunctions.size(); i++) {
            BuiltInFunctionValue function = builtInsHolder.builtInFunctions.get(i);
            builtIns[i] = function;
        }

        // Add the built-in methods
        for (var entry : builtInsHolder.builtInMethods.entrySet()) {
            var methods = new HashMap<String, BuiltInMethodFactory>();
            methods.putAll(builtInsHolder.commonMethods);
            methods.putAll(entry.getValue());
            metaTables.put(entry.getKey(), methods);
        }
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
     * Get a meta table for the given type.
     *
     * @param type The type to get the meta table for.
     * @return The meta table.
     */
    @CompilerDirectives.TruffleBoundary
    public Map<String, BuiltInMethodFactory> getMetaTable(String type) {
        return this.metaTables.get(type);
    }

    public Map<String, Object> getGlobalObjects() {
        return globalObjects;
    }
}
