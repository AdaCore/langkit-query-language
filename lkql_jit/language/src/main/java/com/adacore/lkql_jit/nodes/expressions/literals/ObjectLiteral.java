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
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;


/**
 * This node represents the literal node for an LKQL object
 *
 * @author Hugo GUERRIER
 */
public final class ObjectLiteral extends Expr {

    // ----- Attributes -----

    /**
     * Object ordered keys.
     */
    private final String[] keys;

    // ----- Children -----

    /**
     * Object ordered values.
     */
    @Children
    private Expr[] values;

    // ----- Constructors -----

    /**
     * Create a new object literal node.
     * !!! IMPORTANT !!!
     * Keys and values MUST be in the same order such as: obj.keys[n] = values[n].
     *
     * @param location The location of the node in the source.
     * @param keys     Ordered keys of the object.
     * @param values   Ordered values of the object.
     */
    public ObjectLiteral(
        SourceLocation location,
        String[] keys,
        Expr[] values
    ) {
        super(location);
        this.keys = keys;
        this.values = values;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeObject(frame);
    }

    /**
     * @see com.adacore.lkql_jit.nodes.expressions.Expr#executeObject(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    @ExplodeLoop
    public ObjectValue executeObject(VirtualFrame frame) {
        // Execute the values of the object
        final Object[] values = new Object[this.keys.length];
        for (int i = 0; i < this.keys.length; i++) {
            values[i] = this.values[i].executeGeneric(frame);
        }

        // Return the object value
        return new ObjectValue(this.keys, values);
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
