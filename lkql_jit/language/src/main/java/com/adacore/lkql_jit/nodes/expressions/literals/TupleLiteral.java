/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.TupleValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a tuple literal declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class TupleLiteral extends Expr {

    // ----- Children -----

    /** The expressions contained in the tuple. */
    @Children private final Expr[] exprs;

    // ----- Constructors -----

    /**
     * Create a new tuple literal node.
     *
     * @param location The location of the node in the source.
     * @param exprs The expressions that are inside the tuple.
     */
    public TupleLiteral(SourceLocation location, Expr[] exprs) {
        super(location);
        this.exprs = exprs;
    }

    // ----- Execute methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeTuple(frame);
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeTuple(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public TupleValue executeTuple(VirtualFrame frame) {
        // Evaluate the tuple values
        Object[] values = new Object[this.exprs.length];
        for (int i = 0; i < this.exprs.length; i++) {
            values[i] = this.exprs[i].executeGeneric(frame);
        }

        // Return the tuple value
        return new TupleValue(values);
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
