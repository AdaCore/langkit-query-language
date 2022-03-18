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

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.NullValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;

/**
 * This node represents the indexing operation in the LKQL language (i.e. "test"[1])
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "collection", type = Expr.class)
@NodeChild(value = "index", type = Expr.class)
public abstract class Indexing extends Expr {

    // ----- Attributes -----

    /** If the indexing is safe */
    protected final boolean isSafe;

    // ----- Constructors -----

    /**
     * Create an indexing node with the needed parameters
     *
     * @param location The location of the node in the source
     * @param isSafe If the indexing operation is safe
     */
    protected Indexing(
            SourceLocation location,
            boolean isSafe
    ) {
        super(location);
        this.isSafe = isSafe;
    }

    // ----- Execution methods -----

    /**
     * Get a value in the collection with a long index
     *
     * @param collection The collection
     * @param index The index of the wanted element
     * @return The element
     */
    @Specialization
    protected Object indexIndexable(Indexable collection, long index) {
        try {
            return collection.get((int) index - 1);
        } catch (InvalidIndexException e) {
            if(this.isSafe) {
                return UnitValue.getInstance();
            } else {
                throw LKQLRuntimeException.invalidIndex((int) index, this);
            }
        }
    }

    /**
     * Specialization for the indexing operation on a node
     *
     * @param node The node to get the child from
     * @param index The index of the child to get
     * @return The child or null if there is none
     */
    @Specialization
    protected Object indexNode(Libadalang.AdaNode node, long index) {
        if(index > node.getChildrenCount()) {
            return NullValue.getInstance();
        } else if(index < 1) {
            throw LKQLRuntimeException.invalidIndex((int) index, this);
        }
        return node.getChild((int) index - 1);
    }

    /**
     * Fallback methods when the indexing operation cannot be performed
     *
     * @param collection The collection
     * @param index The index
     */
    @Fallback
    protected void indexError(Object collection, Object index) {
        if(!LKQLTypeSystemGen.isIndexable(collection)) {
            throw LKQLRuntimeException.wrongType(
                    "list, tuple, node or iterator",
                    LKQLTypesHelper.fromJava(collection),
                    this
            );
        } else {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_INTEGER,
                    LKQLTypesHelper.fromJava(index),
                    this
            );
        }
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"isSafe"},
                new Object[]{this.isSafe}
        );
    }

}
