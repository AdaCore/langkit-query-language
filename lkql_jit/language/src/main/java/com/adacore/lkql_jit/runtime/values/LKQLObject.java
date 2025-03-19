//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.bases.ObjectLKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached.Exclusive;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents an object value in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public final class LKQLObject extends ObjectLKQLValue implements LKQLValue {

    // ----- Attributes -----*

    private static final Shape.Builder shapeBuilder = Shape.newBuilder();
    /** The source location where this object was created. This attribute was
     * introduced to provide location information for object instances defined
     * in LKQL rules files. */
    private final SourceSection creationLocation;

    // ----- Constructors -----

    /** Create a new LKQL object with its dynamic shape. */
    public LKQLObject(final Shape shape, SourceSection creationLocation) {
        super(shape);
        this.creationLocation = creationLocation;
    }

    // ----- Class methods -----

    /**
     * Create an LQKL object value from its keys and values. This method doesn't use a cached
     * library so this is not designed for performance critical usages.
     */
    public static LKQLObject createUncached(final Object[] keys, final Object[] values) {
        // NOTE: this LKQL object doesn't have a source location for now
        final LKQLObject res = new LKQLObject(emptyShape(), null);
        for (int i = 0; i < keys.length; i++) {
            uncachedObjectLibrary.put(res, keys[i], values[i]);
        }
        return res;
    }

    /** Get an empty dynamic shape. */
    @CompilerDirectives.TruffleBoundary
    public static Shape emptyShape() {
        return shapeBuilder.build();
    }

    // ----- Value methods -----

    @ExportMessage
    public boolean hasSourceLocation() {
        return creationLocation != null;
    }

    @ExportMessage
    public SourceSection getSourceLocation() {
        return creationLocation;
    }

    /** Exported message to compare two LKQL objects. */
    @ExportMessage
    static class IsIdenticalOrUndefined {

        /** Compare two LKQL objects. */
        @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
        protected static TriState onLKQLObject(
            final LKQLObject left,
            final LKQLObject right,
            @CachedLibrary("left") DynamicObjectLibrary lefts,
            @CachedLibrary("right") DynamicObjectLibrary rights,
            @CachedLibrary(
                limit = Constants.DISPATCHED_LIB_LIMIT
            ) @Exclusive InteropLibrary leftValues,
            @CachedLibrary(
                limit = Constants.DISPATCHED_LIB_LIMIT
            ) @Exclusive InteropLibrary rightValues
        ) {
            return TriState.valueOf(
                objectValueEquals(left, right, lefts, rights, leftValues, rightValues)
            );
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
            @SuppressWarnings("unused") final LKQLObject receiver,
            @SuppressWarnings("unused") final Object other
        ) {
            return TriState.UNDEFINED;
        }
    }

    /** Get the identity hash code for the given object */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(LKQLObject receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string of the object. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffects,
        @CachedLibrary("this") DynamicObjectLibrary thisLibrary,
        @CachedLibrary(
            limit = Constants.DISPATCHED_LIB_LIMIT
        ) @Exclusive InteropLibrary interopLibrary
    ) {
        // Prepare the result string builder and get the keys of the object
        StringBuilder resultBuilder = new StringBuilder("{");
        Object[] keys = thisLibrary.getKeyArray(this);

        // Iterate over keys and add the values
        for (int i = 0; i < keys.length; i++) {
            // Get key/value
            Object key = keys[i];
            Object value = thisLibrary.getOrDefault(this, key, null);

            // Create the string of the value
            String valueString;
            if (value instanceof String) {
                valueString = StringUtils.toRepr((String) interopLibrary.toDisplayString(value));
            } else {
                valueString = (String) interopLibrary.toDisplayString(value);
            }

            // Add the strings to the result
            resultBuilder.append('"').append(key).append('"');
            resultBuilder.append(": ").append(valueString);
            if (i < keys.length - 1) resultBuilder.append(", ");
        }

        // Return the string result
        resultBuilder.append("}");
        return resultBuilder.toString();
    }
}
