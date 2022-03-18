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
import com.adacore.lkql_jit.runtime.values.Pattern;


/**
 * This node represents a regular expression pattern in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class RegexPattern extends ValuePattern {

    // ----- Attributes -----

    /** The regex pattern to match the node text with */
    private final Pattern pattern;

    // ----- Constructors -----

    /**
     * Create a new regex pattern node
     *
     * @param location The location of the node in the source
     * @param regex The regular expression string
     */
    public RegexPattern(
            SourceLocation location,
            String regex
    ) {
        super(location);
        this.pattern = new Pattern(this, regex, true);
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executePattern(com.oracle.truffle.api.frame.VirtualFrame, com.adacore.libadalang.Libadalang.AdaNode) */
    @Override
    public boolean executePattern(VirtualFrame frame, Libadalang.AdaNode node) {
        return !node.isNull() && this.pattern.contains(node.getText());
    }

    /**
     * Execute the pattern on a string
     *
     * @param frame The frame to execute in
     * @param text The text to verify
     * @return The result of the text matching
     */
    public boolean executeOnString(VirtualFrame frame, String text) {
        return pattern.contains(text);
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"pattern"},
                new Object[]{this.pattern}
        );
    }

}
