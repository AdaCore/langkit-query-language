/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins.values.lists;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.interfaces.Indexable;
import com.adacore.lkql_jit.built_ins.values.interfaces.Iterable;
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.built_ins.values.interfaces.Truthy;
import com.adacore.lkql_jit.built_ins.values.iterators.LKQLIterator;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/** This abstract class represents all list like values in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public abstract class BaseLKQLList implements LKQLValue, Iterable, Indexable, Truthy {

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

    /** Tell the interop API that the value has an associated language. */
    @ExportMessage
    public boolean hasLanguage() {
        return true;
    }

    /** Give the LKQL language class to the interop library. */
    @ExportMessage
    public Class<? extends TruffleLanguage<?>> getLanguage() {
        return LKQLLanguage.class;
    }

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
                @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary rightElems) {
            try {
                // Get the left list size and compare it with the right list
                long size = lefts.getArraySize(left);
                if (size != rights.getArraySize(right)) return TriState.FALSE;

                // Then compare each element of the lists
                for (long i = 0; i < size; i++) {
                    Object leftElem = lefts.readArrayElement(left, i);
                    Object rightElem = rights.readArrayElement(right, i);
                    if (leftElems.hasIdentity(leftElem)) {
                        if (!leftElems.isIdentical(leftElem, rightElem, rightElems))
                            return TriState.FALSE;
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
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL list. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(BaseLKQLList receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public Object toDisplayString(
            @SuppressWarnings("unused") final boolean allowSideEffect,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary elems) {
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

    /** Tell the interop library that the value is array like. */
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

    // ----- Override methods -----

    @Override
    public String toString() {
        InteropLibrary listLibrary = InteropLibrary.getUncached(this);
        return (String) listLibrary.toDisplayString(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof BaseLKQLList other)) return false;
        InteropLibrary thisLibrary = InteropLibrary.getUncached(this);
        InteropLibrary otherLibrary = InteropLibrary.getUncached(other);
        return thisLibrary.isIdentical(this, other, otherLibrary);
    }
}
