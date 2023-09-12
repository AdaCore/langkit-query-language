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

import com.adacore.lkql_jit.runtime.Cell;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;


/**
 * This node is the base of all LKQL root nodes.
 *
 * @author Hugo GUERRIER
 */
public abstract class BaseRootNode extends RootNode {

    // ----- Constructors -----

    /**
     * Create a new base root node.
     *
     * @param language        The language instance to link the root node with.
     * @param frameDescriptor The frame descriptor for the root node.
     */
    protected BaseRootNode(
        final TruffleLanguage<?> language,
        final FrameDescriptor frameDescriptor
    ) {
        super(language, frameDescriptor);
    }

    // ----- Instance methods -----

    /**
     * Get the call target as a generic Truffle calling interface.
     *
     * @return The call target.
     */
    public CallTarget getRealCallTarget() {
        return this.getCallTarget();
    }

    /**
     * Initialize the frame slots at empty cells.
     *
     * @param frame The frame to initialize.
     */
    protected void initFrame(VirtualFrame frame) {
        final int slotNumber = frame.getFrameDescriptor().getNumberOfSlots();
        for (int i = 0; i < slotNumber; i++) {
            frame.setObject(i, new Cell());
        }
    }

}
