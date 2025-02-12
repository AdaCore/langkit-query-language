//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.nodes.utils.ImageNode;
import com.adacore.lkql_jit.runtime.values.bases.ObjectLKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.Constants;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents an object value in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public final class LKQLObject extends ObjectLKQLValue implements LKQLValue {

    // ----- Constants -----

    private final TruffleString OPEN_BRACK = TruffleString.fromJavaStringUncached(
        "{",
        Constants.STRING_ENCODING
    );
    private final TruffleString CLOSE_BRACK = TruffleString.fromJavaStringUncached(
        "}",
        Constants.STRING_ENCODING
    );
    private final TruffleString COMMA = TruffleString.fromJavaStringUncached(
        ", ",
        Constants.STRING_ENCODING
    );
    private final TruffleString COLON = TruffleString.fromJavaStringUncached(
        ": ",
        Constants.STRING_ENCODING
    );
    private final TruffleString DOUBLE_QUOTES = TruffleString.fromJavaStringUncached(
        "\"",
        Constants.STRING_ENCODING
    );

    // ----- Constructors -----

    /** Create a new LKQL object with its dynamic shape. */
    public LKQLObject(final Shape shape) {
        super(shape);
    }

    // ----- Class methods -----

    /**
     * Create an LQKL object value from its keys and values. This method doesn't use a cached
     * library so this is not designed for performance critical usages.
     */
    public static LKQLObject createUncached(final Object[] keys, final Object[] values) {
        final LKQLObject res = new LKQLObject(Shape.newBuilder().build());
        for (int i = 0; i < keys.length; i++) {
            uncachedObjectLibrary.put(res, keys[i], values[i]);
        }
        return res;
    }

    // ----- Value methods -----

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
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary leftValues,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary rightValues
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
        @Cached ImageNode imageNode,
        @Cached TruffleString.ConcatNode concatNode,
        @Cached TruffleString.FromJavaStringNode fromJavaStringNode
    ) {
        // Prepare the result string builder and get the keys of the object
        TruffleString res = OPEN_BRACK;
        Object[] keys = thisLibrary.getKeyArray(this);

        // Iterate over keys and add the values
        for (int i = 0; i < keys.length; i++) {
            String key = (String) keys[i];
            Object value = thisLibrary.getOrDefault(this, key, null);
            res = concatNode.execute(
                res,
                concatNode.execute(
                    DOUBLE_QUOTES,
                    concatNode.execute(
                        fromJavaStringNode.execute(key, Constants.STRING_ENCODING),
                        DOUBLE_QUOTES,
                        Constants.STRING_ENCODING,
                        true
                    ),
                    Constants.STRING_ENCODING,
                    true
                ),
                Constants.STRING_ENCODING,
                true
            );
            res = concatNode.execute(
                res,
                concatNode.execute(
                    COLON,
                    imageNode.execute(value),
                    Constants.STRING_ENCODING,
                    true
                ),
                Constants.STRING_ENCODING,
                true
            );
            if (i < keys.length - 1) {
                res = concatNode.execute(res, COMMA, Constants.STRING_ENCODING, true);
            }
        }

        // Return the string result
        res = concatNode.execute(res, CLOSE_BRACK, Constants.STRING_ENCODING, true);
        return res;
    }
}
