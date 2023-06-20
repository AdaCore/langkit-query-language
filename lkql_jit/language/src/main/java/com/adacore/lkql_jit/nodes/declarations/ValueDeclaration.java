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

package com.adacore.lkql_jit.nodes.declarations;


import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents a value declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ValueDeclaration extends Declaration {

    // ----- Attributes -----

    /**
     * Frame slot to place the value in.
     */
    private final int slot;

    // ----- Children -----

    /**
     * The expression representing the value of the variable.
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr value;

    // ----- Constructors -----

    /**
     * Create a new value declaration node.
     *
     * @param location The location of the node in the source.
     * @param slot     The frame slot to place the value in.
     * @param value    The expression representing the value.
     */
    public ValueDeclaration(
        final SourceLocation location,
        final int slot,
        final Expr value
    ) {
        super(location, null);
        this.slot = slot;
        this.value = value;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        FrameUtils.writeLocal(frame, this.slot, this.value.executeGeneric(frame));
        return UnitValue.getInstance();
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[]{"slot"},
            new Object[]{this.slot}
        );
    }

}
