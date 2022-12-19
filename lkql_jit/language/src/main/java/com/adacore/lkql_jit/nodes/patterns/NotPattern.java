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
 * This node represents the negation of a given pattern in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class NotPattern extends ValuePattern {

    // ----- Children -----

    /** The pattern to negate */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ValuePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new not pattern node
     *
     * @param location The location of the node in the source
     * @param pattern The pattern to negate
     */
    public NotPattern(
            SourceLocation location,
            ValuePattern pattern
    ) {
        super(location);
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeNode(com.oracle.truffle.api.frame.VirtualFrame, com.adacore.libadalang.Libadalang.AdaNode) */
    @Override
    public boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node) {
        return !this.pattern.executeNode(frame, node);
    }

    /** @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeString(com.oracle.truffle.api.frame.VirtualFrame, String) */
    @Override
    public boolean executeString(VirtualFrame frame, String str) {
        return !this.pattern.executeString(frame, str);
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
