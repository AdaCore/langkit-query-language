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
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.PropertyRefValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a pattern detail on a field in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class NodePatternField extends NodePatternDetail {

    // ----- Attributes -----

    /**
     * The name of the field to get
     */
    protected final String fieldName;

    // ----- Children -----

    /**
     * The expected value for the field
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected DetailValue expected;

    // ----- Constructors -----

    /**
     * Create a new node pattern field detail node
     *
     * @param location  The location of the node in the source
     * @param fieldName The name of the field to get
     * @param expected  The expected value for the field
     */
    protected NodePatternField(
        SourceLocation location,
        String fieldName,
        DetailValue expected
    ) {
        super(location);
        this.fieldName = fieldName;
        this.expected = expected;
    }

    // ----- Execution methods -----

    /**
     * Execute the field detail with the cached path
     *
     * @param frame       The frame to execute in
     * @param node        The node get the field from
     * @param propertyRef The cached property reference
     * @return True if the detail is valid, false else
     */
    @Specialization(guards = {
        "node == propertyRef.getNode()",
        "propertyRef.getFieldDescription() != null"
    })
    protected boolean fieldCached(
        VirtualFrame frame,
        @SuppressWarnings("unused") Libadalang.AdaNode node,
        @Cached("create(node, fieldName)") PropertyRefValue propertyRef
    ) {
        // Get the value of the field
        Object value = propertyRef.executeAsField(this);

        // Verify if the detail value match
        return this.expected.executeDetailValue(frame, value);
    }

    /**
     * Execute the field detail with the un-cached path
     *
     * @param frame The frame to execute in
     * @param node  The node get the field from
     * @return True if the detail is valid, false else
     */
    @Specialization(replaces = "fieldCached")
    protected boolean fieldUncached(
        VirtualFrame frame,
        Libadalang.AdaNode node
    ) {
        // Get the field property reference
        PropertyRefValue propertyRef = new PropertyRefValue(node, this.fieldName);

        // Verify if the field method is null
        if (propertyRef.getFieldDescription() == null) {
            throw LKQLRuntimeException.noSuchField(this);
        }

        // Execute the field detail
        return this.fieldCached(frame, node, propertyRef);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[]{"fieldName"},
            new Object[]{this.fieldName}
        );
    }

}
