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

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.LKQLLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;


/**
 * This root node represents the root execution of an LKQL program
 *
 * @author Hugo GUERRIER
 */
public final class LKQLRootNode extends RootNode {

    // ----- Attributes -----

    /**
     * The list of nodes representing the program
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private TopLevelList program;

    // ----- Constructors -----

    /**
     * Create a new LKQL root node in order to execute it in Truffle
     *
     * @param program  The LKQL program to execute
     * @param language The reference to the LKQL language instance
     */
    public LKQLRootNode(TopLevelList program, LKQLLanguage language) {
        super(language);
        this.program = program;
    }

    /**
     * Create a new LKQL root node in order to execute it in truffle
     *
     * @param program         The LKQL program to execute
     * @param language        The reference to the LKQL language instance
     * @param frameDescriptor The descriptor of the execution frame
     */
    public LKQLRootNode(TopLevelList program, LKQLLanguage language, FrameDescriptor frameDescriptor) {
        super(language, frameDescriptor);
        this.program = program;
    }

    // ----- Getters -----

    public TopLevelList getProgram() {
        return program;
    }

    // ----- Execution methods -----

    /**
     * Execute the LKQL program that the node contains and return the namespace of the program
     *
     * @param frame The frame to execute in
     * @return The namespace of the LKQL program
     * @see com.oracle.truffle.api.nodes.RootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object execute(VirtualFrame frame) {
        return this.program.executeGeneric(frame);
    }

}
