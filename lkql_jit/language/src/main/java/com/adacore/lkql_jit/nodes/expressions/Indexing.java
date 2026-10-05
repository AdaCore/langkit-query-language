//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.LKQLNull;
import com.adacore.lkql_jit.values.interfaces.Indexable;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents an indexing operation. TODO: support wrap around (#642). */
@NodeChild(value = "collection", type = Expr.class)
@NodeChild(value = "index", type = Expr.class)
public abstract class Indexing extends Expr {

    // ----- Attributes -----

    /** Whether the indexing is safe. */
    protected final boolean isSafe;

    // ----- Constructors -----

    /**
     * Create an indexing node.
     *
     * @param location The location of the node in the source.
     * @param isSafe Whether the indexing operation is safe.
     */
    protected Indexing(SourceSection location, boolean isSafe) {
        super(location);
        this.isSafe = isSafe;
    }

    // ----- Execution methods -----

    /** Specialization for the unsafe indexing operation on null. */
    @Specialization(guards = "isSafe == false")
    protected Object indexNull(LKQLNull nullObj, long index) {
        throw LKQLRuntimeError.nullIndexing(this);
    }

    /** Specialization for the safe indexing operation on null. */
    @Specialization(guards = "isSafe == true")
    protected Object indexNullSafe(LKQLNull nullObj, long index) {
        return LKQLNull.INSTANCE;
    }

    /** Get a value in the collection with a long index. */
    @Specialization
    protected Object indexIndexable(Indexable collection, long index) {
        try {
            return collection.get((int) index);
        } catch (IndexOutOfBoundsException e) {
            throw LKQLRuntimeError.invalidIndex((int) index, this);
        }
    }

    /** Specialization for the indexing operation on a node. */
    @Specialization
    protected Object indexNode(LangkitSupport.NodeInterface node, long index) {
        if (index < 0 || index >= node.getChildrenCount()) {
            throw LKQLRuntimeError.invalidIndex((int) index, this);
        }
        LangkitSupport.NodeInterface res = node.getChild((int) index);
        return res.isNone() ? LKQLNull.INSTANCE : res;
    }

    /** Fallback methods when the indexing operation cannot be performed. */
    @Fallback
    protected void indexError(Object collection, Object index) {
        if (!LKQLTypeSystemGen.isIndexable(collection)) {
            throw LKQLRuntimeError.wrongType(
                "Indexable",
                LKQLTypesHelper.fromJava(collection),
                this
            );
        } else {
            throw LKQLRuntimeError.wrongType(
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
            new String[] { "isSafe" },
            new Object[] { this.isSafe }
        );
    }
}
