//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;

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
     */
    protected InClause(SourceSection location) {
        super(location);
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
        Iterator iterator = iterable.iterator();
        while (iterator.hasNext()) {
            if (ObjectUtils.equals(elem, iterator.next())) return true;
        }
        return false;
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
            getRight()
        );
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
