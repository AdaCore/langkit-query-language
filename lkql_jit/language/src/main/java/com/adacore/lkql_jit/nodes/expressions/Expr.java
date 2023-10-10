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

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.*;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.*;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Nullish;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.math.BigInteger;

/**
 * This node is the base of all expressions in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class Expr extends LKQLNode {

    // ----- Constructors -----

    /**
     * Create a new expression node.
     *
     * @param location The location of the node in the source.
     */
    protected Expr(SourceLocation location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * Execute the expression as the unit value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as unit.
     * @throws UnexpectedResultException If the node cannot be evaluated as a unit value.
     */
    public LKQLUnit executeUnit(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLUnit(executeGeneric(frame));
    }

    /**
     * Execute the expression as a nullish value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as nullish.
     * @throws UnexpectedResultException If the node cannot be evaluated as a nullish value.
     */
    public Nullish executeNullish(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectNullish(executeGeneric(frame));
    }

    /**
     * Execute the expression as a boolean.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a boolean.
     * @throws UnexpectedResultException If the node cannot be evaluated as a boolean.
     */
    public boolean executeBoolean(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectBoolean(executeGeneric(frame));
    }

    /**
     * Execute the expression as a long.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a long.
     * @throws UnexpectedResultException If the node cannot be evaluated as a long.
     */
    public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLong(executeGeneric(frame));
    }

    /**
     * Execute the expression as a big integer.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a big integer.
     * @throws UnexpectedResultException If the node cannot be evaluated as a big integer.
     */
    public BigInteger executeBigInteger(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectBigInteger(executeGeneric(frame));
    }

    /**
     * Execute the expression as a string.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a string.
     * @throws UnexpectedResultException If the node cannot be evaluated as a string.
     */
    public String executeString(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectString(executeGeneric(frame));
    }

    /**
     * Execute the expression as a function value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a function value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a function.
     */
    public LKQLFunction executeFunction(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLFunction(executeGeneric(frame));
    }

    /**
     * Execute the expression as a property reference value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a property reference value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a property reference.
     */
    public PropertyRefValue executePropertyRef(VirtualFrame frame)
            throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectPropertyRefValue(executeGeneric(frame));
    }

    /**
     * Execute the expression as a selector value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a selector value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a selector.
     */
    public SelectorValue executeSelector(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectSelectorValue(executeGeneric(frame));
    }

    /**
     * Execute the expression as a tuple value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a tuple value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a tuple.
     */
    public LKQLTuple executeTuple(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLTuple(executeGeneric(frame));
    }

    /**
     * Execute the expression as a list value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a list value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a list.
     */
    public LKQLList executeList(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLList(executeGeneric(frame));
    }

    /**
     * Execute the expression as an indexable value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as an indexable value.
     * @throws UnexpectedResultException If the node cannot be evaluated as an indexable.
     */
    public Indexable executeIndexable(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectIndexable(executeGeneric(frame));
    }

    /**
     * Execute the expression as an iterable value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as an iterable value.
     * @throws UnexpectedResultException If the node cannot be evaluated as an iterable.
     */
    public Iterable executeIterable(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectIterable(executeGeneric(frame));
    }

    /**
     * Execute the expression as an object value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as an object value.
     * @throws UnexpectedResultException If the node cannot be evaluated as an object.
     */
    public LKQLObject executeObject(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLObject(executeGeneric(frame));
    }

    /**
     * Execute the expression as a namespace value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a namespace value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a namespace.
     */
    public LKQLNamespace executeNamespace(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLNamespace(executeGeneric(frame));
    }

    /**
     * Execute the expression as a LKQL value.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as a lkql value.
     * @throws UnexpectedResultException If the node cannot be evaluated as a lkql value.
     */
    public LKQLValue executeValue(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectLKQLValue(executeGeneric(frame));
    }

    /**
     * Execute the expression as an ada node.
     *
     * @param frame The frame for execution.
     * @return The result of the node execution as an ada node.
     * @throws UnexpectedResultException If the node cannot be evaluated as an ada node.
     */
    public Libadalang.AdaNode executeNode(VirtualFrame frame) throws UnexpectedResultException {
        return LKQLTypeSystemGen.expectAdaNode(executeGeneric(frame));
    }
}
