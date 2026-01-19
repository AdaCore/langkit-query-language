//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.values.interfaces.Indexable;
import com.adacore.lkql_jit.values.interop.LKQLValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached.Exclusive;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents a tuple in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLTuple extends LKQLValue implements Indexable {

    // ----- Attributes -----

    /** The content of the tuple. */
    private final Object[] content;

    // ----- Constructors -----

    /** Create a new tuple with its content. */
    public LKQLTuple(final Object[] content) {
        this.content = content;
    }

    // ----- Value methods -----

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public Object toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffect,
        @CachedLibrary(
            limit = Constants.DISPATCHED_LIB_LIMIT
        ) @Exclusive InteropLibrary interopLibrary
    ) {
        // Prepare the result
        StringBuilder resultBuilder = new StringBuilder("(");

        // Iterate over the list values
        for (int i = 0; i < this.content.length; i++) {
            Object elem = this.content[i];

            // Get the element string
            String elemString;
            if (elem instanceof String) {
                elemString = StringUtils.toRepr((String) interopLibrary.toDisplayString(elem));
            } else {
                elemString = (String) interopLibrary.toDisplayString(elem);
            }

            // Add the element string to the result
            resultBuilder.append(elemString);
            if (i < this.content.length - 1) resultBuilder.append(", ");
        }

        // Return the result
        resultBuilder.append(")");
        return resultBuilder.toString();
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
    public Object get(long index) throws IndexOutOfBoundsException {
        return this.content[(int) index];
    }

    @Override
    public Object[] getContent() {
        return this.content;
    }

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof LKQLTuple other)) return false;
        if (this.content.length != other.content.length) return false;
        for (int i = 0; i < this.content.length; i++) {
            if (!ObjectUtils.equals(this.content[i], other.content[i])) return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return ObjectUtils.hashCode(this.content);
    }
}
