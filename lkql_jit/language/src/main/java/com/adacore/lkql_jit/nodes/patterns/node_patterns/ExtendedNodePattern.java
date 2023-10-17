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

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.nodes.patterns.ValuePattern;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a pattern that access fields or properties of nodes.
 *
 * @author Hugo GUERRIER
 */
public final class ExtendedNodePattern extends NodePattern {

    // ----- Children -----

    /** The pattern to extend. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ValuePattern basePattern;

    /** The details representing the extension. */
    @Children private final NodePatternDetail[] details;

    // ----- Constructors -----

    /**
     * Create a new extended node pattern node.
     *
     * @param location The location of the node in the source.
     * @param basePattern The base pattern to extend.
     * @param details The extensions for the base pattern.
     */
    public ExtendedNodePattern(
            SourceLocation location, ValuePattern basePattern, NodePatternDetail[] details) {
        super(location);
        this.basePattern = basePattern;
        this.details = details;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.patterns.BasePattern#executeNode(com.oracle.truffle.api.frame.VirtualFrame,
     *     com.adacore.libadalang.Libadalang.AdaNode)
     */
    @Override
    public boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node) {
        // Test the base pattern
        if (this.basePattern.executeNode(frame, node)) {
            // Verify all details
            for (NodePatternDetail detail : this.details) {
                if (!detail.executeDetail(frame, node)) return false;
            }

            // Return the success
            return true;
        }

        // Return the failure
        return false;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
