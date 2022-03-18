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

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node is a helper to write a frame argument to a local frame slot
 *
 * @author Hugo GUERRIER
 */
public final class WriteArg extends LKQLNode {

    // ----- Attributes -----

    /** The local slot to write the argument in */
    private final int slot;

    // ----- Constructors -----

    /**
     * Create a new write argument node
     *
     * @param slot The slot of the frame to write in
     */
    public WriteArg(
            int slot
    ) {
        super(null);
        this.slot = slot;
    }

    // ----- Getters -----

    public int getSlot() {
        return this.slot;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the argument writing with the arg index
     *
     * @param frame The frame write the argument in
     * @param argIndex The argument index
     */
    public void executeWrite(VirtualFrame frame, int argIndex) {
        frame.setObject(this.slot, frame.getArguments()[argIndex]);
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"slot"},
                new Object[]{this.slot}
        );
    }

}
