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

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.NamespaceValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.utils.util_classes.Stack;
import com.oracle.truffle.api.CompilerDirectives;

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

    /**
     * The global values
     */
    private final Stack<Object[]> valuesStack;

    /**
     * The global names
     */
    private final Stack<String[]> namesStack;

    /**
     * The namespace stack
     */
    private final Stack<NamespaceValue> namespaceStack;

    /**
     * The number of symbol to export
     */
    private final Stack<Integer> exportNumberStack;

    /**
     * The imported LKQL rules
     */
    private final Stack<Map<String, ObjectValue>> checkersStack;

    /**
     * The array containing the built-in functions and selectors
     */
    private final Object[] builtIns;

    /**
     * The meta tables that contains built-in methods
     */
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
        this.namespaceStack = new Stack<>();
        this.exportNumberStack = new Stack<>();
        this.checkersStack = new Stack<>();
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

    public Stack<NamespaceValue> getNamespaceStack() {
        return namespaceStack;
    }

    // ----- Class methods -----

    /**
     * This function add a global scope for a script execution
     *
     * @param slotNumber   The number of used slots
     * @param exportNumber The number of slot to export
     */
    @CompilerDirectives.TruffleBoundary
    public void initScope(int slotNumber, int exportNumber) {
        this.valuesStack.push(new Object[slotNumber - this.builtIns.length]);
        this.namesStack.push(new String[slotNumber - this.builtIns.length]);
        this.exportNumberStack.push(exportNumber - this.builtIns.length);
        this.checkersStack.push(new HashMap<>());
    }

    /**
     * This function finalize the current global scope, at the end of a script execution
     */
    @CompilerDirectives.TruffleBoundary
    public void finalizeScope() {
        this.valuesStack.pop();
        this.namesStack.pop();
        this.exportNumberStack.pop();
        this.checkersStack.pop();
    }

    /**
     * Push the namespace on the global context to execute the module code
     *
     * @param namespaceValue The namespace value to push on
     */
    @CompilerDirectives.TruffleBoundary
    public void pushNamespace(NamespaceValue namespaceValue) {
        this.valuesStack.push(namespaceValue.getValues());
        this.namesStack.push(namespaceValue.getNames());
        this.namespaceStack.push(namespaceValue);
        this.exportNumberStack.push(namespaceValue.getValues().length);
        this.checkersStack.push(namespaceValue.getCheckers());
    }

    /**
     * Pop the last pushed namespace
     */
    @CompilerDirectives.TruffleBoundary
    public void popNamespace() {
        this.valuesStack.pop();
        this.namesStack.pop();
        this.namespaceStack.pop();
        this.exportNumberStack.pop();
        this.checkersStack.pop();
    }

    /**
     * Get a variable from the global context
     *
     * @param slot The slot of the variable
     * @return The variable value or null if it doesn't exists
     */
    public Object get(int slot) {
        if (slot >= 0) {
            if (slot < this.builtIns.length) return this.builtIns[slot];
            slot -= this.builtIns.length;
            if (slot < this.valuesStack.peek().length) return this.valuesStack.peek()[slot];
        }
        return null;
    }

    /**
     * Set a global variable with its slot
     *
     * @param slot  The slot of the variable
     * @param name  The name of the variable
     * @param value The value of the variable
     */
    public void set(int slot, String name, Object value) {
        this.namesStack.peek()[slot - this.builtIns.length] = name;
        this.valuesStack.peek()[slot - this.builtIns.length] = value;
    }

    /**
     * Get the LKQL checkers of the current global scope
     *
     * @return The LKQL checkers
     */
    public Map<String, ObjectValue> getCheckers() {
        return this.checkersStack.peek();
    }

    /**
     * Add the given LKQL checker in the global values
     *
     * @param name    The name of the checker
     * @param checker The object representing the checker
     */
    @CompilerDirectives.TruffleBoundary
    public void addChecker(String name, ObjectValue checker) {
        this.checkersStack.peek().put(name, checker);
    }

    /**
     * Add all checkers from a namespace value to the current scope
     *
     * @param namespaceValue The namespace to get the checkers from
     */
    @CompilerDirectives.TruffleBoundary
    public void addCheckers(NamespaceValue namespaceValue) {
        this.checkersStack.peek().putAll(namespaceValue.getCheckers());
    }

    /**
     * Set a built-in value, this function is only used in built-in factory
     *
     * @param slot  The slot of the built-in
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
     * @param type    The type of the meta table
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
            this.checkersStack.peek()
        );

        // Set the function namespace
        for (Object value : res.getValues()) {
            if (LKQLTypeSystemGen.isFunctionValue(value)) {
                LKQLTypeSystemGen.asFunctionValue(value).setNamespace(res);
            } else if (LKQLTypeSystemGen.isLazyCollection(value)) {
                LKQLTypeSystemGen.asLazyCollection(value).setNamespace(res);
            }
        }

        // Return the result
        return res;
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return "GlobalScope{" +
            "\n\texportNumber=" + exportNumberStack.peek() +
            "\n\tvalues=" + Arrays.toString(valuesStack.peek()) +
            "\n\tnames=" + Arrays.toString(namesStack.peek()) +
            "\n\tbuiltIns=" + Arrays.toString(builtIns) +
            "\n\tcheckers=" + this.checkersStack.peek() +
            "\n}";
    }

}