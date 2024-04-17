//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.list_comprehension;

import com.adacore.lkql_jit.built_ins.values.interfaces.Iterable;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This node represents an association in a list comprehension in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ComprehensionAssoc extends LKQLNode {

    // ----- Attributes -----

    /** The name of the binding value. */
    private final String name;

    /** The slot to put the value in. */
    private final int slot;

    // ----- Children -----

    /** The expression that contains the collection to iterate on. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr collection;

    // ----- Constructors -----

    /**
     * Create a new comprehension association.
     *
     * @param location The location of the node in the source.
     * @param name The name of the binding value.
     * @param slot The slot to put the value in.
     * @param collection The collection expression.
     */
    public ComprehensionAssoc(SourceLocation location, String name, int slot, Expr collection) {
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

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the collection expression and return its result.
     *
     * @param frame The frame to execute in.
     * @return The collection result of the expression result.
     */
    public Iterable executeCollection(VirtualFrame frame) {
        try {
            return this.collection.executeIterable(frame);
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_ITERABLE,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.collection);
        }
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"name", "slot"}, new Object[] {this.name, this.slot});
    }
}
