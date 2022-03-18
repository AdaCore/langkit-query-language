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

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;


/**
 * This node is the base of all LKQL root nodes
 *
 * @author Hugo GUERRIER
 */
public abstract class BaseRootNode extends RootNode {

    // ----- Attributes -----

    /** The closure of the root node */
    protected final Closure closure;

    // ----- Children -----

    /** The argument writing nodes */
    @Children
    protected WriteArg[] argumentWriting;

    // ----- Constructors -----

    /**
     * Create a new root node
     *
     * @param language The language instance to link the root node with
     * @param frameDescriptor The frame descriptor for the root node
     * @param closure The execution closure of the root node
     */
    protected BaseRootNode(
            TruffleLanguage<?> language,
            FrameDescriptor frameDescriptor,
            Closure closure
    ) {
        super(language, frameDescriptor);
        this.closure = closure;
    }

    // ----- Getters -----

    public Closure getClosure() {
        return this.closure;
    }

    // ----- Internal methods -----

    /**
     * Create the arg writing nodes from the function parameter slots
     *
     * @param slots The function parameter slots
     * @return The argument writing nodes
     */
    protected WriteArg[] createArgWritings(int[] slots) {
        // Create the result array with the correct length
        WriteArg[] res = new WriteArg[slots.length];

        // Create the writing nodes
        for(int i = 0 ; i < slots.length ; i++) {
            res[i] = new WriteArg(slots[i]);
        }

        // Return the result
        return res;
    }

    /**
     * Instantiate the closure if it's not null
     *
     * @param frame The frame to instantiate the closure in
     */
    protected void instantiateClosure(VirtualFrame frame) {
        if(this.closure != null) {
            this.closure.instantiate(frame.materialize());
        }
    }

    /**
     * Instantiate the arguments in the local frame
     *
     * @param frame The frame to instantiate the args in
     */
    protected void instantiateArgs(VirtualFrame frame) {
        for(int i = 0 ; i < this.argumentWriting.length ; i++) {
            this.argumentWriting[i].executeWrite(frame, i);
        }
    }

    // ----- Class methods -----

    /**
     * Get the call target as a generic Truffle calling interface
     *
     * @return The call target
     */
    public CallTarget getRealCallTarget() {
        return this.getCallTarget();
    }

}
