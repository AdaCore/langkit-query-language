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

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.nodes.patterns.RegexPattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a pattern detail value in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class DetailPattern extends DetailValue {

    // ----- Children -----

    /** The pattern to verify for the detail value */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new pattern detail
     *
     * @param location The location of the node in the source
     * @param pattern The pattern to verify
     */
    public DetailPattern(
            SourceLocation location,
            BasePattern pattern
    ) {
        super(location);
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.patterns.node_patterns.DetailValue#executeDetailValue(com.oracle.truffle.api.frame.VirtualFrame, java.lang.Object) */
    @Override
    public boolean executeDetailValue(VirtualFrame frame, Object value) {
        // If the pattern is a regex pattern and the value is a string
        if(this.pattern instanceof RegexPattern regexPattern && LKQLTypeSystemGen.isString(value)) {
            return regexPattern.executeOnString(frame, LKQLTypeSystemGen.asString(value));
        }

        // Verify that the value is a node
        if(!LKQLTypeSystemGen.isAdaNode(value)) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.ADA_NODE,
                    LKQLTypesHelper.fromJava(value),
                    this
            );
        }

        // Execute the pattern with the node
        return this.pattern.executePattern(frame, LKQLTypeSystemGen.asAdaNode(value));
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
