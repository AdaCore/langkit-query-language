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

package com.adacore.lkql_jit.nodes.patterns.chained_patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.nodes.patterns.SelectorCall;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a selector in a chained pattern
 *
 * @author Hugo GUERRIER
 */
public final class SelectorLink extends ChainedPatternLink {

    // ----- Children -----

    /**
     * The selector call to perform during link execution
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private SelectorCall selectorCall;

    // ----- Constructors -----

    /**
     * Create a new selector link node
     *
     * @param location     The location of the node in the source
     * @param pattern      The pattern to verify
     * @param selectorCall The selector call
     */
    public SelectorLink(
        SourceLocation location,
        BasePattern pattern,
        SelectorCall selectorCall
    ) {
        super(location, pattern);
        this.selectorCall = selectorCall;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.patterns.chained_patterns.ChainedPatternLink#executeLink(com.oracle.truffle.api.frame.VirtualFrame, com.adacore.libadalang.Libadalang.AdaNode)
     */
    @Override
    public Libadalang.AdaNode[] executeLink(VirtualFrame frame, Libadalang.AdaNode node) {
        return (Libadalang.AdaNode[]) this.selectorCall.executeFiltering(frame, node, this.pattern).getContent();
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
