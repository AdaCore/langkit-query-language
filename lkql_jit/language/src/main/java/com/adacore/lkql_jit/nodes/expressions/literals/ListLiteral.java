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

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a list literal node in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ListLiteral extends Expr {

    // ----- Children -----

    /**
     * The list expressions.
     */
    @Children
    private final Expr[] exprs;

    // ----- Constructors -----

    /**
     * Create a new list literal node.
     *
     * @param location The location of the node in the source.
     * @param exprs    The expressions inside the list.
     */
    public ListLiteral(
        SourceLocation location,
        Expr[] exprs
    ) {
        super(location);
        this.exprs = exprs;
    }

    // ----- Execute methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeList(frame);
    }

    /**
     * @see com.adacore.lkql_jit.nodes.expressions.Expr#executeList(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public ListValue executeList(VirtualFrame frame) {
        // Evaluate the list content
        Object[] values = new Object[this.exprs.length];
        for (int i = 0; i < this.exprs.length; i++) {
            values[i] = this.exprs[i].executeGeneric(frame);
        }

        // Return the list value
        return new ListValue(values);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
