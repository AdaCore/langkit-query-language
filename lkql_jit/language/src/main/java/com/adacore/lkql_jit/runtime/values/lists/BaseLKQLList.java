//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLIterator;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached.Exclusive;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This abstract class represents all list like values in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public abstract class BaseLKQLList implements Iterable, Indexable, TruffleObject {

    // ----- Constructors -----

    protected BaseLKQLList() {}

    // ----- Basic list methods to fulfill -----

    /** Get the size of the list as a long value. */
    public abstract long size();

    /**
     * Get the element at the given position in the list.
     *
     * @throws IndexOutOfBoundsException If the provided index is not in the list bounds.
     */
    @CompilerDirectives.TruffleBoundary
    public abstract Object get(long i) throws IndexOutOfBoundsException;

    /** Get the iterator for the list. */
    public abstract LKQLIterator iterator();

    // ----- Value methods -----

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public Object toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffect,
        @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) @Exclusive InteropLibrary elems
    ) {
        // Prepare the result
        StringBuilder resultBuilder = new StringBuilder("[");

        // Iterate over the list values
        for (int i = 0; i < this.size(); i++) {
            Object elem = this.get(i);

            // Get the element string
            String elemString;
            if (elem instanceof String) {
                elemString = StringUtils.toRepr((String) elems.toDisplayString(elem));
            } else {
                elemString = (String) elems.toDisplayString(elem);
            }

            // Add the element string to the result
            resultBuilder.append(elemString);
            if (i < this.size() - 1) resultBuilder.append(", ");
        }

        // Return the result
        resultBuilder.append("]");
        return resultBuilder.toString();
    }

    /** Tell the interop API that the list can be cast to a boolean. */
    @ExportMessage
    public boolean isBoolean() {
        return true;
    }

    /** Get the boolean representation of the list. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public boolean asBoolean() {
        try {
            this.get(0);
            return true;
        } catch (IndexOutOfBoundsException e) {
            return false;
        }
    }

    /** Tell the interop library that tuple has array elements. */
    @ExportMessage
    public boolean hasArrayElements() {
        return true;
    }

    /** Get the array size for the interop library. */
    @ExportMessage
    public long getArraySize() {
        return this.size();
    }

    /** Tell the interop library if the wanted index is readable. */
    @ExportMessage
    public boolean isArrayElementReadable(long index) {
        try {
            this.get((int) index);
            return true;
        } catch (IndexOutOfBoundsException e) {
            return false;
        }
    }

    /** Get the array element of the given index. */
    @ExportMessage
    public Object readArrayElement(long index) {
        return this.get(index);
    }

    /** Tell the interop library that the list has an iterator object. */
    @ExportMessage
    public boolean hasIterator() {
        return true;
    }

    /** Get the iterator for the list. */
    @ExportMessage
    public Object getIterator() {
        return this.iterator();
    }

    // ----- Indexable required methods -----

    @Override
    public Object[] getContent() {
        Object[] res = new Object[(int) this.size()];
        for (int i = 0; i < this.size(); i++) {
            res[i] = this.get(i);
        }
        return res;
    }

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof BaseLKQLList other)) return false;
        var size = this.size();
        if (size != other.size()) return false;
        for (int i = 0; i < size; i++) {
            if (!ObjectUtils.equals(this.get(i), other.get(i))) return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return ObjectUtils.hashCode(this.getContent());
    }
}
