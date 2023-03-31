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

package com.adacore.lkql_jit.nodes.declarations.variables;

import com.adacore.lkql_jit.nodes.declarations.DeclAnnotation;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a local variable declaration in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class ValDeclLocal extends ValDecl {

    // ----- Constructors -----

    /**
     * Create a new local variable declaration with the slot of it
     *
     * @param location   The location of the node in the source
     * @param annotation The annotation related to the variable declaration
     * @param name       The name of the variable
     * @param slot       The slot to put the variable in
     * @param value      The value of the variable
     */
    public ValDeclLocal(
        SourceLocation location,
        DeclAnnotation annotation,
        String name,
        int slot,
        Expr value
    ) {
        super(location, annotation, name, slot, value);
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Put the value in the frame
        frame.setObject(this.slot, this.value.executeGeneric(frame));

        // Return the unit value
        return UnitValue.getInstance();
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[]{"name", "slot"},
            new Object[]{this.name, this.slot}
        );
    }

}
