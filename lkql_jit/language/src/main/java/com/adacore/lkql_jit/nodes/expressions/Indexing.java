//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLTuple;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the indexing operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "collection", type = Expr.class)
@NodeChild(value = "index", type = Expr.class)
public abstract class Indexing extends Expr {

    // ----- Attributes -----

    /** Whether the indexing is safe. */
    protected final boolean isSafe;

    // ----- Constructors -----

    /**
     * Create an indexing node with the needed parameters.
     *
     * @param location The location of the node in the source.
     * @param isSafe Whether the indexing operation is safe.
     */
    protected Indexing(SourceSection location, boolean isSafe) {
        super(location);
        this.isSafe = isSafe;
    }

    // ----- Execution methods -----

    /**
     * Execute the indexing operation on a truffle tuple.
     *
     * @param tuple The tuple to index
     * @param index The index of the element to get.
     * @return The element at the given index in the tuple, unit if the index is invalid and the
     *     indexing is safe else raise a runtime error.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object indexTuple(
        final LKQLTuple tuple,
        final long index,
        @CachedLibrary("tuple") InteropLibrary tupleLibrary
    ) {
        try {
            return tupleLibrary.readArrayElement(tuple, index - 1);
        } catch (InvalidArrayIndexException e) {
            if (this.isSafe) {
                return LKQLUnit.INSTANCE;
            } else {
                throw LKQLRuntimeException.invalidIndex((int) index, this);
            }
        } catch (UnsupportedMessageException e) {
            throw LKQLRuntimeException.fromJavaException(e, this);
        }
    }

    /**
     * Get a value in the collection with a long index.
     *
     * @param collection The collection to get the element from.
     * @param index The index of the wanted element.
     * @return The element of the collection.
     */
    @Specialization
    protected Object indexIndexable(Indexable collection, long index) {
        try {
            return collection.get((int) index - 1);
        } catch (InvalidIndexException e) {
            if (this.isSafe) {
                return LKQLUnit.INSTANCE;
            } else {
                throw LKQLRuntimeException.invalidIndex((int) index, this);
            }
        }
    }

    /**
     * Specialization for the indexing operation on a node.
     *
     * @param node The node to get the child from.
     * @param index The index of the child to get.
     * @return The child or null if there is none.
     */
    @Specialization
    protected Object indexNode(LangkitSupport.NodeInterface node, long index) {
        if (index > node.getChildrenCount()) {
            return LKQLNull.INSTANCE;
        } else if (index < 1) {
            throw LKQLRuntimeException.invalidIndex((int) index, this);
        }
        LangkitSupport.NodeInterface res = node.getChild((int) index - 1);
        return res.isNone() ? LKQLNull.INSTANCE : res;
    }

    /**
     * Fallback methods when the indexing operation cannot be performed.
     *
     * @param collection The maybe invalid collection.
     * @param index The maybe invalid index.
     */
    @Fallback
    protected void indexError(Object collection, Object index) {
        if (!LKQLTypeSystemGen.isIndexable(collection)) {
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

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "isSafe" },
                new Object[] { this.isSafe }
            );
    }
}
