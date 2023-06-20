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

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.nodes.patterns.SelectorCall;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a pattern detail on a selector in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NodePatternSelector extends NodePatternDetail {

    // ----- Children ------

    /**
     * The selector call for the detail.
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private SelectorCall call;

    /**
     * The pattern to check from the selector.
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new node pattern selector detail node.
     *
     * @param location The location of the node in the source.
     * @param call     The selector call.
     * @param pattern  The pattern to check the selector.
     */
    public NodePatternSelector(
        SourceLocation location,
        SelectorCall call,
        BasePattern pattern
    ) {
        super(location);
        this.call = call;
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.patterns.node_patterns.NodePatternDetail#executeDetail(com.oracle.truffle.api.frame.VirtualFrame, com.adacore.libadalang.Libadalang.AdaNode)
     */
    @Override
    public boolean executeDetail(VirtualFrame frame, Libadalang.AdaNode node) {
        return this.call.executeVerification(frame, node, this.pattern);
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
