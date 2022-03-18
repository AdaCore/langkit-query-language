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

package com.adacore.lkql_jit.parser;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;

import java.util.*;


/**
 * This class represents a scope for the LKQL source parsing
 *
 * @author Hugo GUERRIER
 */
public final class Scope {

    // ----- Attributes -----

    /** The maps containing the bindings from the variable names to identifiers */
    private final LinkedList<Map<String, Integer>> bindingsStack;

    /** The counter stack to handle lexical scopes */
    private final LinkedList<Integer> counterStack;

    /** The maximum counter */
    private int globalSize;

    /** The local frame descriptor builder stack to handle semantic scopes */
    private final LinkedList<FrameDescriptor.Builder> frameDescriptorStack;

    /** The map that associate the frame to their lexical scopes */
    private final Map<FrameDescriptor.Builder, List<Map<String, Integer>>> frameBindings;

    // ----- Constructors -----

    /**
     * Create a new scope
     */
    public Scope() {
        this.bindingsStack = new LinkedList<>();
        this.bindingsStack.add(new HashMap<>());
        this.counterStack = new LinkedList<>();
        this.counterStack.add(0);
        this.globalSize = 0;
        this.frameDescriptorStack = new LinkedList<>();
        this.frameBindings = new IdentityHashMap<>();
    }

    // ----- Getters -----

    public int getGlobalSize() {
        return this.globalSize;
    }

    public int getExportNumber() {
        return this.counterStack.get(0);
    }

    public boolean isGlobal() {
        return this.frameDescriptorStack.isEmpty();
    }

    public FrameDescriptor buildDescriptor() {
        return this.frameDescriptorStack.get(this.frameDescriptorStack.size() - 1).build();
    }

    // ----- Internal methods -----

    /**
     * Get the next available global slot and increase the counter
     *
     * @return The next slot
     */
    private int nextGlobalSlot() {
        // Get the result and increase the counter
        int res = this.counterStack.getLast();
        this.counterStack.set(this.counterStack.size() - 1, res + 1);

        // Set the max counter
        if(this.globalSize < res + 1) {
            this.globalSize = res + 1;
        }

        // Return the result
        return res;
    }

    /**
     * This method clone a frame descriptor builder with all its slot and their types
     *
     * @param builder The builder you want to clone, if it's null, a new empty builder is returned
     * @return The new builder
     */
    private static FrameDescriptor.Builder cloneFrameDescriptorBuilder(FrameDescriptor.Builder builder) {
        // If the builder to clone is null, just return an empty builder
        if(builder == null) return FrameDescriptor.newBuilder();

        // Prepare the result and the frame descriptor to copy
        FrameDescriptor build = builder.build();
        FrameDescriptor.Builder res = FrameDescriptor.newBuilder();

        // Copy the frame descriptor
        for(int i = 0 ; i < build.getNumberOfSlots() ; i++) {
            res.addSlot(build.getSlotKind(i), build.getSlotName(i), build.getSlotInfo(i));
        }

        // Return the result
        return res;
    }

    // ----- Class methods -----

    /**
     * Add a variable in the current scope and return its slot
     *
     * @param name The name of the variable
     * @return The slot of the variable
     */
    public int addVariable(String name) {
        // Prepare the result
        int slot;

        // If the scope is in the global state, add the variable to the global bindings
        if(this.isGlobal()) {
            slot = this.nextGlobalSlot();
        }

        // Else the scope is currently local
        else {
            slot = this.frameDescriptorStack.getLast().addSlot(FrameSlotKind.Object, null, null);
        }

        // Put the variable in the current bindings
        this.bindingsStack.getLast().put(name, slot);

        // Return the result
        return slot;
    }

    /**
     * Get the slot for a given variable name
     *
     * @param name The name of the variable
     * @return The variable slot or -1 if the variable doesn't exist
     */
    public int getVariable(String name) {
        // Prepare the result
        int res = -1;

        // Get the result from the more local scope to the more global
        for(int i = this.bindingsStack.size() - 1 ; i >= 0 ; i--)  {
            res = this.bindingsStack.get(i).getOrDefault(name, -1);
            if(res > -1) return res;
        }

        // Return the result
        return res;
    }

    /**
     * Get the slot for a given variable name only in the current scope
     *
     * @param name The name of the variable
     * @return The slot of the variable in the current scope or -1
     */
    public int getVariableInScope(String name) {
        return this.bindingsStack.getLast().getOrDefault(name, -1);
    }

    /**
     * Get if a variable is in a semantic local scope
     *
     * @param name The variable to look for
     * @return True if the variable is in a local semantic scope
     */
    public boolean isSemanticLocal(String name) {
        // Iterate on frame
        for(int i = this.frameDescriptorStack.size() - 1 ; i >= 0 ; i--) {
            for(Map<String, Integer> bindings : this.frameBindings.get(this.frameDescriptorStack.get(i))) {
                if(bindings.containsKey(name)) return true;
            }
        }

        // Return the default result
        return false;
    }

    /**
     * Open a lexical scope
     */
    public void openLexical() {
        Map<String, Integer> newBindings = new HashMap<>();
        if(!this.frameDescriptorStack.isEmpty()) {
            this.frameBindings.get(this.frameDescriptorStack.getLast()).add(newBindings);
        }
        this.bindingsStack.add(newBindings);
        this.counterStack.add(this.counterStack.getLast());
    }

    /**
     * Close the lexical scope
     */
    public void closeLexical() {
        if(!this.frameDescriptorStack.isEmpty()) {
            List<Map<String, Integer>> frameBindings = this.frameBindings.get(this.frameDescriptorStack.getLast());
            frameBindings.remove(frameBindings.size() - 1);
        }
        this.bindingsStack.removeLast();
        this.counterStack.removeLast();
    }

    /**
     * Open a semantic scope
     */
    public void openSemantic() {
        FrameDescriptor.Builder previousBuilder = this.frameDescriptorStack.size() == 0 ?
                null :
                this.frameDescriptorStack.get(this.frameDescriptorStack.size() - 1);
        FrameDescriptor.Builder newBuilder = cloneFrameDescriptorBuilder(previousBuilder);
        this.frameDescriptorStack.add(newBuilder);
        this.frameBindings.put(newBuilder, new LinkedList<>());
        this.openLexical();
    }

    /**
     * Close the semantic scope
     */
    public void closeSemantic() {
        this.closeLexical();
        FrameDescriptor.Builder oldBuilder = this.frameDescriptorStack.removeLast();
        this.frameBindings.remove(oldBuilder);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        List<String> frames = new ArrayList<>();
        for(FrameDescriptor.Builder builder : this.frameDescriptorStack) {
            FrameDescriptor frameDescriptor = builder.build();
            StringBuilder frameImage = new StringBuilder("{");
            for(int i = 0 ; i < frameDescriptor.getNumberOfSlots() ; i++) {
                frameImage.append(i).append(": ").append(frameDescriptor.getSlotKind(i));
                if(i < frameDescriptor.getNumberOfSlots() - 1) {
                    frameImage.append(", ");
                }
            }
            frameImage.append("}");
            frames.add(frameImage.toString());
        }

        return "Scope{\n" +
                "\tbindings = " + bindingsStack + "\n" +
                "\tcounters = " + counterStack + "\n" +
                "\tframes = " + frames + "\n" +
                "\tframes_bindings = " + frameBindings + "\n" +
                "\tglobal_size = " + globalSize + "\n" +
                "}";
    }
}
