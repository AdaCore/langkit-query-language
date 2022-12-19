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

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.runtime.values.PropertyRefValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;


/**
 * This node represents a pattern detail on a property in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class NodePatternProperty extends NodePatternDetail {

    // ----- Attributes -----

    /** The name of the property to call */
    protected final String propertyName;

    // ----- Children -----

    /** The list of the argument for the property call */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected ArgList argList;

    /** The expected value for the property call */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected DetailValue expected;

    // ----- Constructors -----

    /**
     * Create a new pattern detail on a property
     *
     * @param location The token location in the source
     * @param propertyName The name of the property to call
     * @param argList The arguments for the property call
     * @param expected The expected value of the property call
     */
    public NodePatternProperty(
            SourceLocation location,
            String propertyName,
            ArgList argList,
            DetailValue expected
    ) {
        super(location);
        this.propertyName = propertyName;
        this.argList = argList;
        this.expected = expected;
    }

    // ----- Execution methods -----

    /**
     * Execute the property detail with the cached path
     *
     * @param frame The frame to execute in
     * @param node The node get the property from
     * @param propertyRef The cached property reference
     * @return True if the detail is valid, false else
     */
    @Specialization(guards = {
            "node == propertyRef.getNode()",
            "propertyRef.getFieldDescription() != null"
    })
    protected boolean propertyCached(
            VirtualFrame frame,
            @SuppressWarnings("unused") Libadalang.AdaNode node,
            @Cached("create(node, propertyName)") PropertyRefValue propertyRef
    ) {
        // Evaluate the arguments
        Object[] arguments = new Object[this.argList.getArgs().length];
        for(int i = 0 ; i < arguments.length ; i++) {
            arguments[i] = this.argList.getArgs()[i].getArgExpr().executeGeneric(frame);
        }

        // Get the property result
        Object value = propertyRef.execute(this, this.argList, arguments);

        // Verify the pattern
        return this.expected.executeDetailValue(frame, value);
    }

    /**
     * Execute the property detail with the un-cached path
     *
     * @param frame The frame to execute in
     * @param node The node get the property from
     * @return True if the detail is valid, false else
     */
    @Specialization(replaces = "propertyCached")
    protected boolean propertyUncached(
            VirtualFrame frame,
            Libadalang.AdaNode node
    ) {
        // Get the property methods
        PropertyRefValue propertyRef = new PropertyRefValue(node, this.propertyName);

        // Test if the property is null
        if(propertyRef.getFieldDescription() == null) {
            throw LKQLRuntimeException.noSuchField(
                    this.propertyName,
                    node,
                    this
            );
        }

        // Return the result
        return this.propertyCached(
                frame,
                node,
                propertyRef
        );
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"propertyName"},
                new Object[]{this.propertyName}
        );
    }

}
