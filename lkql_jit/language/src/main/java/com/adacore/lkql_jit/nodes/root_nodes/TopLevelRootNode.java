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

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This root node represents the root execution of an LKQL program.
 *
 * @author Hugo GUERRIER
 */
public final class TopLevelRootNode extends BaseRootNode {

    // ----- Attributes -----

    /** The list of nodes representing the program. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private TopLevelList program;

    // ----- Constructors -----

    /**
     * Create a new LKQL top level root node.
     *
     * @param program The LKQL program to execute.
     * @param language The reference to the LKQL language instance.
     */
    public TopLevelRootNode(final TopLevelList program, final LKQLLanguage language) {
        super(language, program.getFrameDescriptor());
        this.program = program;
    }

    // ----- Execution methods -----

    /**
     * Execute the LKQL program and return the namespace, result of this program execution. This
     * root node expects 1 argument: - boolean checkerMode: If the top level list node is in checker
     * mode. Default is false.
     *
     * @param frame The frame to execute in.
     * @return The namespace of the LKQL program.
     * @see com.oracle.truffle.api.nodes.RootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object execute(VirtualFrame frame) {
        // Get the execution arguments and perform pre-computing actions
        Object[] arguments = frame.getArguments();
        final boolean checkerMode = arguments.length > 0 && (boolean) arguments[0];

        // If the checker mode is activated add all rule imports
        if (checkerMode) {
            this.program.addRuleImports();
        }

        // Initialize the frame
        this.initFrame(frame);

        // Execute the program
        return this.program.executeGeneric(frame);
    }
}
