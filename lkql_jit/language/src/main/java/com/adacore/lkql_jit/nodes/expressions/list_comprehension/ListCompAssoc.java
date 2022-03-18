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

package com.adacore.lkql_jit.nodes.expressions.list_comprehension;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This node represents an association in a list comprehension in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class ListCompAssoc extends LKQLNode {

    // ----- Attributes -----

    /** The name of the binding value */
    private final String name;

    /** The slot to put the value in */
    private final int slot;

    // ----- Children -----

    /** The expression that contains the collection to iterate on */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr collection;

    // ----- Constructors -----

    /**
     * Create a new comprehension association
     *
     * @param location The location of the node in the source
     * @param name The name of the binding value
     * @param slot The slot to put the value in
     * @param collection The collection expression
     */
    public ListCompAssoc(
            SourceLocation location,
            String name,
            int slot,
            Expr collection
    ) {
        super(location);
        this.name = name;
        this.slot = slot;
        this.collection = collection;
    }

    // ----- Getters -----

    public String getName() {
        return name;
    }

    public int getSlot() {
        return slot;
    }

    public Expr getCollection() {
        return collection;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the collection expression and return its result
     *
     * @param frame The frame to execute in
     * @return The collection result of the expression result
     */
    public Iterable executeCollection(VirtualFrame frame) {
        try {
            return this.collection.executeIterable(frame);
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_ITERABLE,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.collection
            );
        }
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"name", "slot"},
                new Object[]{this.name, this.slot}
        );
    }

}
