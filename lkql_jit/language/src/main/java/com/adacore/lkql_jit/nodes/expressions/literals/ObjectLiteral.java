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

import com.adacore.lkql_jit.built_ins.values.LKQLObject;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;

/**
 * This node represents the literal node for an LKQL object.
 *
 * @author Hugo GUERRIER
 */
public final class ObjectLiteral extends Expr {

    // ----- Attributes -----

    /** Object ordered keys. */
    private final String[] keys;

    /** The shape of the dynamic object. */
    private final Shape shape;

    // ----- Children -----

    /** Object ordered values. */
    @Children private final Expr[] values;

    /** Object library to insert values in the result. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private DynamicObjectLibrary objectLibrary;

    // ----- Constructors -----

    /**
     * Create a new object literal node. !!! IMPORTANT !!! Keys and values MUST be in the same order
     * such as: obj.keys[n] = values[n].
     *
     * @param location The location of the node in the source.
     * @param keys Ordered keys of the object.
     * @param values Ordered values of the object.
     */
    public ObjectLiteral(final SourceLocation location, final String[] keys, final Expr[] values) {
        super(location);
        this.keys = keys;
        this.shape = Shape.newBuilder().build();
        this.values = values;
        this.objectLibrary = DynamicObjectLibrary.getUncached();
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeObject(frame);
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeObject(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    @ExplodeLoop
    public LKQLObject executeObject(final VirtualFrame frame) {
        // Create the result object
        LKQLObject res = new LKQLObject(this.shape);
        for (int i = 0; i < this.keys.length; i++) {
            this.objectLibrary.put(res, this.keys[i], this.values[i].executeGeneric(frame));
        }

        // Return the new LKQL object
        return res;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(final int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
