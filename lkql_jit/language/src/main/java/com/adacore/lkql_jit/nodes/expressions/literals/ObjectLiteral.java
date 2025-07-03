//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.literals;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLObject;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import com.oracle.truffle.api.source.SourceSection;

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
    @Children
    private final Expr[] values;

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
    public ObjectLiteral(final SourceSection location, final String[] keys, final Expr[] values) {
        super(location);
        this.keys = keys;
        this.shape = LKQLObject.emptyShape();
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
        LKQLObject res = new LKQLObject(this.shape, this.location);
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

    // ----- Inner classes -----

    /** Wrapper node ensuring values of "@-objects" are lists. */
    @NodeChild(value = "wrappedExpr", type = Expr.class)
    public abstract static class AtObjectValueWrapper extends Expr {

        // ----- Constructors -----

        public AtObjectValueWrapper(SourceSection location) {
            super(location);
        }

        // ----- Specializations -----

        @Specialization
        protected LKQLList onList(LKQLList list) {
            return list;
        }

        @Fallback
        protected LKQLList onOthers(Object obj) {
            return new LKQLList(new Object[] { obj });
        }

        // ----- Override methods -----

        @Override
        public String toString(int indentLevel) {
            return this.nodeRepresentation(indentLevel);
        }
    }
}
