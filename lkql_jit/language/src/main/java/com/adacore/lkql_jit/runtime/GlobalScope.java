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

package com.adacore.lkql_jit.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.NamespaceValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.utils.util_classes.Stack;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;


/**
 * This class represents the global values scope for an LKQL script execution
 *
 * @author Hugo GUERRIER
 */
public final class GlobalScope {

    // ----- Attributes -----

    /** The global values */
    private final Stack<Object[]> valuesStack;

    /** The global names */
    private final Stack<String[]> namesStack;

    /** The number of symbol to export */
    private final Stack<Integer> exportNumberStack;

    /** The node checkers for the current script */
    private final Stack<ArrayList<ObjectValue>> nodeCheckersStack;

    /** The unit checker for the current script */
    private final Stack<ArrayList<ObjectValue>> unitCheckersStack;

    /** The array containing the built-in functions and selectors */
    private final Object[] builtIns;

    /** The meta tables that contains built-in methods */
    private final Map<String, Map<String, BuiltInFunctionValue>> metaTables;

    // ----- Constructors -----

    /**
     * Create a new global scope
     *
     * @param buildInNb The number of built-in functions
     */
    public GlobalScope(
            int buildInNb
    ) {
        this.valuesStack = new Stack<>();
        this.namesStack = new Stack<>();
        this.exportNumberStack = new Stack<>();
        this.nodeCheckersStack = new Stack<>();
        this.unitCheckersStack = new Stack<>();
        this.builtIns = new Object[buildInNb];
        this.metaTables = new HashMap<>();
    }

    // ----- Getters -----

    /**
     * Get the size of the global values stack
     *
     * @return The size
     */
    public int getStackSize() {
        return this.valuesStack.size();
    }

    // ----- Class methods -----

    /**
     * This function add a global scope for a script execution
     *
     * @param slotNumber The number of used slots
     * @param exportNumber The number of slot to export
     */
    public void initScope(int slotNumber, int exportNumber) {
        this.valuesStack.push(new Object[slotNumber - this.builtIns.length]);
        this.namesStack.push(new String[slotNumber - this.builtIns.length]);
        this.exportNumberStack.push(exportNumber - this.builtIns.length);
        this.nodeCheckersStack.push(new ArrayList<>());
        this.unitCheckersStack.push(new ArrayList<>());
    }

    /**
     * This function finalize the current global scope, at the end of a script execution
     */
    public void finalizeScope() {
        this.valuesStack.pop();
        this.namesStack.pop();
        this.exportNumberStack.pop();
        this.nodeCheckersStack.pop();
        this.unitCheckersStack.pop();
    }

    /**
     * Push the namespace on the global context to execute the module code
     *
     * @param namespaceValue The namespace value to push on
     */
    public void pushNamespace(NamespaceValue namespaceValue) {
        this.valuesStack.push(namespaceValue.getValues());
        this.namesStack.push(namespaceValue.getNames());
        this.exportNumberStack.push(namespaceValue.getValues().length);
        this.nodeCheckersStack.push(namespaceValue.getNodeCheckers());
        this.unitCheckersStack.push(namespaceValue.getUnitCheckers());
    }

    /**
     * Get a variable from the global context
     *
     * @param slot The slot of the variable
     * @return The variable value or null if it doesn't exists
     */
    public Object get(int slot) {
        if(slot >= 0) {
            if(slot < this.builtIns.length) return this.builtIns[slot];
            slot -= this.builtIns.length;
            if(slot < this.valuesStack.peek().length) return this.valuesStack.peek()[slot];
        }
        return null;
    }

    /**
     * Set a global variable with its slot
     *
     * @param slot The slot of the variable
     * @param name The name of the variable
     * @param value The value of the variable
     */
    public void set(int slot, String name, Object value) {
        this.namesStack.peek()[slot - this.builtIns.length] = name;
        this.valuesStack.peek()[slot - this.builtIns.length] = value;
    }

    /**
     * Get the node checker of the current global scope in an array
     *
     * @return The node checkers
     */
    @CompilerDirectives.TruffleBoundary
    public ObjectValue[] getNodeChecker() {
        return this.nodeCheckersStack.peek().toArray(new ObjectValue[0]);
    }

    /**
     * Add a node checker to the global scope
     *
     * @param checker The checker to add
     */
    public void addNodeChecker(ObjectValue checker) {
        this.nodeCheckersStack.peek().add(checker);
    }

    /**
     * Get the unit checkers of the current global scope in an array
     *
     * @return The unit checkers
     */
    public ObjectValue[] getUnitCheckers() {
        return this.unitCheckersStack.peek().toArray(new ObjectValue[0]);
    }

    /**
     * Add a unit checker to the global scope
     *
     * @param checker The checker to add
     */
    public void addUnitChecker(ObjectValue checker) {
        this.unitCheckersStack.peek().add(checker);
    }

    /**
     * Add all checkers from a namespace value to the current scope
     *
     * @param namespaceValue The namespace to get the checkers from
     */
    public void addCheckers(NamespaceValue namespaceValue) {
        for(int i = 0 ; i < namespaceValue.getNodeCheckers().size() ; i++) {
            this.nodeCheckersStack.peek().add(namespaceValue.getNodeCheckers().get(i));
        }
        for(int i = 0 ; i < namespaceValue.getUnitCheckers().size() ; i++) {
            this.unitCheckersStack.peek().add(namespaceValue.getUnitCheckers().get(i));
        }
    }

    /**
     * Set a built-in value, this function is only used in built-in factory
     *
     * @param slot The slot of the built-in
     * @param value The value
     */
    public void setBuiltIn(int slot, Object value) {
        this.builtIns[slot] = value;
    }

    /**
     * Get a meta table for the given type
     *
     * @param type The type to get the meta table for
     * @return The meta table
     */
    @CompilerDirectives.TruffleBoundary
    public Map<String, BuiltInFunctionValue> getMetaTable(String type) {
        return this.metaTables.get(type);
    }

    /**
     * Put a new meta table
     *
     * @param type The type of the meta table
     * @param methods The methods for the type
     */
    @CompilerDirectives.TruffleBoundary
    public void putMetaTable(String type, Map<String, BuiltInFunctionValue> methods) {
        this.metaTables.put(type, methods);
    }

    /**
     * Export the global scope to a namespace value
     *
     * @return The namespace value from the global scope
     */
    public NamespaceValue export() {
        // Create the result
        NamespaceValue res = new NamespaceValue(
                Arrays.copyOfRange(this.namesStack.peek(), 0, this.exportNumberStack.peek()),
                Arrays.copyOfRange(this.valuesStack.peek(), 0, this.exportNumberStack.peek()),
                this.nodeCheckersStack.peek(),
                this.unitCheckersStack.peek()
        );

        // Set the function namespace
        for(Object value : res.getValues()) {
            if(LKQLTypeSystemGen.isFunctionValue(value)) {
                LKQLTypeSystemGen.asFunctionValue(value).setNamespace(res);
            }
        }

        // Return the result
        return res;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "GlobalScope{" +
                "\n\texportNumber=" + exportNumberStack.peek() +
                "\n\tvalues=" + Arrays.toString(valuesStack.peek()) +
                "\n\tnames=" + Arrays.toString(namesStack.peek()) +
                "\n\tbuiltIns=" + Arrays.toString(builtIns) +
                "\n}";
    }

}
