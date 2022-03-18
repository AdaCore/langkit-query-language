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
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;


/**
 * This node represents the literal node for an LKQL object
 *
 * @author Hugo GUERRIER
 */
public final class ObjectLiteral extends Expr {

    // ----- Children -----

    /** The list of the associations in the object literal */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ObjectAssocList assocList;

    // ----- Constructors -----

    /**
     * Create a new object literal node
     *
     * @param location The location of the node in the source
     * @param assocList The list of the association in the object
     */
    public ObjectLiteral(
            SourceLocation location,
            ObjectAssocList assocList
    ) {
        super(location);
        this.assocList = assocList;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeObject(frame);
    }

    /** @see com.adacore.lkql_jit.nodes.expressions.Expr#executeObject(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public ObjectValue executeObject(VirtualFrame frame) {
        // Get the associations
        Object[][] assocs = this.assocList.executeAssocList(frame);

        // Return the object value
        return new ObjectValue((String[]) assocs[0], assocs[1]);
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
