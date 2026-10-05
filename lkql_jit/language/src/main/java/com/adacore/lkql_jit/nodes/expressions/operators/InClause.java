//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents an inclusion clause. */
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

    /** Execute the inclusion clause, return true if the element is in the iterable. */
    @Specialization
    protected boolean inIterable(Object elem, Iterable iterable) {
        Iterator iterator = iterable.iterator();
        while (iterator.hasNext()) {
            if (ObjectUtils.equals(elem, iterator.next())) return true;
        }
        return false;
    }

    /** Fallback method when user try to do an inclusion clause on a non-iterable value. */
    @Fallback
    protected void notIterable(@SuppressWarnings("unused") Object elem, Object notIterable) {
        throw LKQLRuntimeError.wrongType(
            LKQLTypesHelper.LKQL_LIST,
            LKQLTypesHelper.fromJava(notIterable),
            getRight()
        );
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
