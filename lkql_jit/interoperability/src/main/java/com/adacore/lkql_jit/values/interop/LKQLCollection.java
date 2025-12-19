//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.values.interfaces.Indexable;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents the base of collection like values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLCollection extends LKQLValue implements Iterable, Indexable {

    // ----- Value methods -----

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public Object toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffect,
        @CachedLibrary(
            limit = Constants.DISPATCHED_LIB_LIMIT
        ) @Cached.Exclusive InteropLibrary elems
    ) {
        // Prepare the result
        StringBuilder resultBuilder = new StringBuilder("[");

        // Iterate over the list values
        for (int i = 0; i < this.size(); i++) {
            Object elem = this.get(i);

            // Get the element string
            String elemString;
            if (elem instanceof String) {
                elemString = Utils.toRepr((String) elems.toDisplayString(elem));
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
        if (!(o instanceof LKQLCollection other)) return false;
        var size = this.size();
        if (size != other.size()) return false;
        for (int i = 0; i < size; i++) {
            if (!Utils.eq(this.get(i), other.get(i))) return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        return Utils.hashCode(this.getContent());
    }
}
