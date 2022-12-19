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

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.runtime.values.PropertyRefValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.libadalang.Libadalang;


/**
 * This node represents a field link in a chained pattern in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class FieldLink extends ChainedPatternLink {

    // ----- Attributes -----

    /** The name of the field to query */
    protected final String fieldName;

    // ----- Constructors -----

    /**
     * Create a new field link node
     *
     * @param location The location of the node in the source
     * @param pattern The pattern to verify
     * @param fieldName The name of the field to query
     */
    protected FieldLink(
            SourceLocation location,
            BasePattern pattern,
            String fieldName
    ) {
        super(location, pattern);
        this.fieldName = fieldName;
    }

    // ----- Execution methods -----

    /**
     * Execute the field link with the cached strategy
     *
     * @param frame The frame to execute the link in
     * @param node The node get the field from
     * @param propertyRef The cached property reference
     * @return The result of the link
     */
    @Specialization(guards = {
            "node == propertyRef.getNode()",
            "propertyRef.getFieldDescription() != null"
    })
    protected Libadalang.AdaNode[] fieldCached(
            VirtualFrame frame,
            @SuppressWarnings("unused") Libadalang.AdaNode node,
            @Cached("create(node, fieldName)") PropertyRefValue propertyRef
    ) {
        // Get the value of the field
        Object value = propertyRef.executeAsField(this);

        // Do the pattern filtering
        return this.doPatternFiltering(frame, value);
    }

    /**
     * Execute the field link with the un-cached strategy
     *
     * @param frame The frame to execute the link in
     * @param node The node get the field from
     * @return The result of the link
     */
    @Specialization(replaces = "fieldCached")
    protected Libadalang.AdaNode[] fieldUnached(
            VirtualFrame frame,
            Libadalang.AdaNode node
    ) {
        // Get the field method
        PropertyRefValue propertyRef = new PropertyRefValue(node, this.fieldName);

        // Verify if the field method is null
        if(propertyRef.getFieldDescription() == null) {
            throw LKQLRuntimeException.noSuchField(
                    this.fieldName,
                    node,
                    this
            );
        }

        // Execute the field detail
        return this.fieldCached(frame, node, propertyRef);
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"fieldName"},
                new Object[]{this.fieldName}
        );
    }

}
