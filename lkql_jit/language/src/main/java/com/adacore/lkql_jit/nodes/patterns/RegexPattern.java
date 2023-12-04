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

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.values.LKQLNull;
import com.adacore.lkql_jit.built_ins.values.LKQLPattern;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a regular expression pattern in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class RegexPattern extends ValuePattern {

    // ----- Attributes -----

    /** The regex pattern to match the node text with. */
    private final LKQLPattern pattern;

    // ----- Constructors -----

    /**
     * Create a new regex pattern node.
     *
     * @param location The location of the node in the source.
     * @param regex The regular expression string.
     */
    public RegexPattern(SourceLocation location, String regex) {
        super(location);
        this.pattern = new LKQLPattern(this, regex, true);
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.patterns.BasePattern#executeNode(com.oracle.truffle.api.frame.VirtualFrame,
     *     com.adacore.libadalang.Libadalang.AdaNode)
     */
    @Override
    public boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node) {
        return this.pattern.contains(node.getText()) && node != LKQLNull.INSTANCE;
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.patterns.BasePattern#executeString(com.oracle.truffle.api.frame.VirtualFrame,
     *     String)
     */
    @Override
    public boolean executeString(VirtualFrame frame, String str) {
        return pattern.contains(str);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"pattern"}, new Object[] {this.pattern});
    }
}
