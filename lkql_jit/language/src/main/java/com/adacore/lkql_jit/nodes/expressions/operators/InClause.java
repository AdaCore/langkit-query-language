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

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.built_ins.values.interfaces.Iterable;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

/**
 * This node represents the "in" clause in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class InClause extends BinOp {

    // ----- Constructors -----

    /**
     * Create an "in" clause node.
     *
     * @param location The location of the node in the source.
     * @param leftLocation The location of the left node.
     * @param rightLocation The location of the right node.
     */
    protected InClause(
            SourceLocation location, DummyLocation leftLocation, DummyLocation rightLocation) {
        super(location, leftLocation, rightLocation);
    }

    // ----- Execution methods -----

    /**
     * Execute the "in" clause, return true if the element is in the collection.
     *
     * @param elem The element to search in the list.
     * @param iterable The list to search in.
     * @return True the iterable value contains the element, false else.
     */
    @Specialization
    protected boolean inIterable(Object elem, Iterable iterable) {
        return iterable.contains(elem);
    }

    /**
     * Fallback method when user try to do an "in" clause on a non-iterable value.
     *
     * @param elem The element to search in the list.
     * @param notIterable The non-iterable value.
     */
    @Fallback
    protected void notIterable(@SuppressWarnings("unused") Object elem, Object notIterable) {
        throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_LIST,
                LKQLTypesHelper.fromJava(notIterable),
                this.rightLocation);
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
