//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.nodes.utils.ImageNode;
import com.adacore.lkql_jit.runtime.values.bases.ArrayLKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.utils.Constants;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents a tuple in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLTuple extends ArrayLKQLValue implements Indexable {

    // ----- Constants -----

    private static final TruffleString OPEN_BRACK = TruffleString.fromJavaStringUncached(
        "(",
        Constants.STRING_ENCODING
    );
    private static final TruffleString CLOSE_BRACK = TruffleString.fromJavaStringUncached(
        ")",
        Constants.STRING_ENCODING
    );
    private static final TruffleString COMMA = TruffleString.fromJavaStringUncached(
        ", ",
        Constants.STRING_ENCODING
    );

    // ----- Attributes -----

    /** The content of the tuple. */
    private final Object[] content;

    // ----- Constructors -----

    /** Create a new tuple with its content. */
    public LKQLTuple(final Object[] content) {
        this.content = content;
    }

    // ----- Value methods -----

    /** Exported message to compare two tuples. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {

        /** Compare two LKQL tuples. */
        @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
        public static TriState onTuple(
            final LKQLTuple left,
            final LKQLTuple right,
            @CachedLibrary("left") InteropLibrary lefts,
            @CachedLibrary("right") InteropLibrary rights,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary leftElems,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary rightElems
        ) {
            return TriState.valueOf(
                arrayValueEquals(left, right, lefts, rights, leftElems, rightElems)
            );
        }

        /** Do the comparison with another element. */
        @Fallback
        public static TriState onOther(
            @SuppressWarnings("unused") final LKQLTuple receiver,
            @SuppressWarnings("unused") final Object other
        ) {
            return TriState.UNDEFINED;
        }
    }

    /** Get the identity hash code for the given LKQL tuple */
    @ExportMessage
    public static int identityHashCode(
        LKQLTuple receiver,
        @CachedLibrary("receiver") InteropLibrary receivers,
        @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary elems
    ) {
        return arrayValueHashCode(receiver, receivers, elems);
    }

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public TruffleString toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffect,
        @Cached TruffleString.ConcatNode concatNode,
        @Cached ImageNode imageNode
    ) {
        // Prepare the result
        TruffleString res = OPEN_BRACK;

        // Iterate over the list values
        for (int i = 0; i < this.content.length; i++) {
            Object elem = this.content[i];
            res = concatNode.execute(res, imageNode.execute(elem), Constants.STRING_ENCODING, true);
            if (i < this.content.length - 1) {
                res = concatNode.execute(res, COMMA, Constants.STRING_ENCODING, true);
            }
        }

        // Return the result
        res = concatNode.execute(res, CLOSE_BRACK, Constants.STRING_ENCODING, true);
        return res;
    }

    /** Tell the interop library that tuple has array elements. */
    @ExportMessage
    public boolean hasArrayElements() {
        return true;
    }

    /** Get the array size for the interop library. */
    @ExportMessage
    public long getArraySize() {
        return this.content.length;
    }

    /** Tell the interop library if the wanted index is readable. */
    @ExportMessage
    public boolean isArrayElementReadable(final long index) {
        return index < this.content.length && index >= 0;
    }

    /** Get the array element of the given index. */
    @ExportMessage
    public Object readArrayElement(final long index) throws InvalidArrayIndexException {
        try {
            return this.content[(int) index];
        } catch (IndexOutOfBoundsException e) {
            throw InvalidArrayIndexException.create(index, e);
        }
    }

    // ----- Indexable methods -----

    @Override
    public Object get(long index) throws InvalidIndexException {
        try {
            return this.content[(int) index];
        } catch (IndexOutOfBoundsException e) {
            throw new InvalidIndexException();
        }
    }

    @Override
    public Object[] getContent() {
        return this.content;
    }
}
