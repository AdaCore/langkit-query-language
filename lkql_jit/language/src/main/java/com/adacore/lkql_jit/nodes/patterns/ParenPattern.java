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

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.libadalang.Libadalang;


/**
 * This node represents a parented pattern in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class ParenPattern extends ValuePattern {

    // ----- Children -----

    /** The pattern inside the parenthesis */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new parented pattern node
     *
     * @param location The location of the node in the source
     * @param pattern The pattern inside the parenthesis
     */
    public ParenPattern(
            SourceLocation location,
            BasePattern pattern
    ) {
        super(location);
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executePattern(com.oracle.truffle.api.frame.VirtualFrame, com.adacore.libadalang.Libadalang.AdaNode) */
    @Override
    public boolean executePattern(VirtualFrame frame, Libadalang.AdaNode node) {
        return this.pattern.executePattern(frame, node);
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
