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

package com.adacore.lkql_jit.nodes.expressions.literals.object;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;


/**
 * This node represents an object association list in LKQL
 *
 * @author Hugo GUERRIER
 */
public final class ObjectAssocList extends LKQLNode {

    // ----- Children -----

    /** The associations of the object */
    @Children
    private final ObjectAssoc[] assocs;

    // ----- Constructors -----

    /**
     * Create an object association list node
     *
     * @param location The location of the node in the source
     * @param assocs The associations
     */
    public ObjectAssocList(
            SourceLocation location,
            ObjectAssoc[] assocs
    ) {
        super(location);
        this.assocs = assocs;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)  */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the association list and return an array containing the keys and values
     *
     * @param frame The frame to execute in
     * @return An array containing the keys as first element and the values as second element
     */
    public Object[][] executeAssocList(VirtualFrame frame) {
        // Evaluate the object values
        String[] keys = new String[this.assocs.length];
        Object[] values = new Object[this.assocs.length];
        for(int i = 0 ; i < this.assocs.length ; i++) {
            keys[i] = this.assocs[i].getKey();
            values[i] = this.assocs[i].executeAssoc(frame);
        }

        // Return the result
        return new Object[][]{keys, values};
    }

    // ------ Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
