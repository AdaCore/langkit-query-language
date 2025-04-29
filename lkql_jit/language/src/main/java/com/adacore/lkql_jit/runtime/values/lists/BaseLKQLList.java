//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.bases.ArrayLKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.interfaces.Truthy;
import com.adacore.lkql_jit.runtime.values.iterators.LKQLIterator;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/** This abstract class represents all list like values in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public abstract class BaseLKQLList extends ArrayLKQLValue implements Iterable, Indexable, Truthy {

    // ----- Constructors -----

    protected BaseLKQLList() {}

    // ----- Basic list methods to fulfill -----

    /** Get the size of the list as a long value. */
    public abstract long size();

    /**
     * Get the element at the given position in the list.
     *
     * @throws InvalidIndexException If the provided index is not in the list bounds.
     */
    public abstract Object get(long i) throws InvalidIndexException;

    /** Get the iterator for the list. */
    public abstract LKQLIterator iterator();

    // ----- Value methods -----

    /** Exported message to compare two lists. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {

        /** Compare two LKQL lists. */
        @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
        public static TriState onList(
            final BaseLKQLList left,
            final BaseLKQLList right,
            @CachedLibrary("left") InteropLibrary lefts,
            @CachedLibrary("right") InteropLibrary rights,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary leftElems,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary rightElems
        ) {
            try {
                // Get the left list size and compare it with the right list
                long size = lefts.getArraySize(left);
                if (size != rights.getArraySize(right)) return TriState.FALSE;

                // Then compare each element of the lists
                for (long i = 0; i < size; i++) {
                    Object leftElem = lefts.readArrayElement(left, i);
                    Object rightElem = rights.readArrayElement(right, i);
                    if (leftElems.hasIdentity(leftElem)) {
                        if (
                            !leftElems.isIdentical(leftElem, rightElem, rightElems)
                        ) return TriState.FALSE;
                    } else {
                        if (!ObjectUtils.equals(leftElem, rightElem)) return TriState.FALSE;
                    }
                }

                // If we get here, lists are equal
                return TriState.TRUE;
            } catch (Exception e) {
                throw LKQLRuntimeException.shouldNotHappen("Lists comparison");
            }
        }

        /** Do the comparison with another element. */
        @Fallback
        public static TriState onOther(
            @SuppressWarnings("unused") final BaseLKQLList receiver,
            @SuppressWarnings("unused") final Object other
        ) {
            return TriState.UNDEFINED;
        }
    }

    /** Get the identity hash code for the given LKQL list */
    @ExportMessage
    public static int identityHashCode(
        BaseLKQLList receiver,
        @CachedLibrary("receiver") InteropLibrary receivers,
        @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary elems
    ) {
        return arrayValueHashCode(receiver, receivers, elems);
    }

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public Object toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffect,
        @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary elems
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
    public boolean asBoolean() {
        return this.isTruthy();
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
        } catch (InvalidIndexException e) {
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

    // ----- Truthy required values -----

    @Override
    public boolean isTruthy() {
        try {
            this.get(0);
            return true;
        } catch (InvalidIndexException e) {
            return false;
        }
    }
}
