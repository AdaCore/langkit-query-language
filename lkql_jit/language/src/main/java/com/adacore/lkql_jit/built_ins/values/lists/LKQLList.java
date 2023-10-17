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
import com.adacore.lkql_jit.built_ins.values.iterators.LKQLListIterator;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;
import java.util.Objects;

/** This abstract class represents all list like values in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLList implements TruffleObject, Iterable, Indexable, Truthy {

    // ----- Constructors -----

    /** The protected constructor. */
    protected LKQLList() {}

    // ----- List required methods -----

    /** Get the size of the list. */
    public abstract long size();

    /**
     * Get the list element at the given index.
     *
     * @throws InvalidIndexException If the given index is invalid.
     */
    public abstract Object get(int i) throws InvalidIndexException;

    /** Get the list as a pure Java array. */
    public abstract Object[] asArray();

    /** Get a new iterator on the list. */
    public LKQLListIterator iterator() {
        return new LKQLListIterator(this);
    }

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
        @Specialization
        public static TriState onList(final LKQLList left, final LKQLList right) {
            if (left.internalEquals(right)) return TriState.TRUE;
            else return TriState.FALSE;
        }

        /** Do the comparison with another element. */
        @Fallback
        public static TriState onOther(
                @SuppressWarnings("unused") final LKQLList receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL list. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(LKQLList receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public Object toDisplayString(
            @SuppressWarnings("unused") final boolean allowSideEffect,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary interopLibrary) {
        // Prepare the result
        StringBuilder resultBuilder = new StringBuilder("[");

        // Iterate over the list values
        for (int i = 0; i < this.size(); i++) {
            Object elem = this.get(i);

            // Get the element string
            String elemString;
            if (elem instanceof String) {
                elemString = StringUtils.toRepr((String) interopLibrary.toDisplayString(elem));
            } else {
                elemString = (String) interopLibrary.toDisplayString(elem);
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
        return this.get((int) index);
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

    // ---- LKQL value methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean internalEquals(final LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof LKQLList other)) return false;
        if (this.size() != other.size()) return false;
        for (int i = 0; i < this.size(); i++) {
            Object mineObject = this.get(i);
            Object hisObject = other.get(i);
            if ((mineObject instanceof LKQLValue mine) && (hisObject instanceof LKQLValue his)) {
                if (!mine.internalEquals(his)) return false;
            } else {
                if (!Objects.equals(mineObject, hisObject)) return false;
            }
        }
        return true;
    }

    @Override
    public boolean isTruthy() {
        try {
            this.get(0);
            return true;
        } catch (InvalidIndexException e) {
            return false;
        }
    }

    @Override
    public Object[] getContent() {
        return this.asArray();
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean contains(Object elem) {
        final LKQLListIterator iterator = this.iterator();
        while (iterator.hasNext()) {
            Object val = iterator.next();
            if (elem.equals(val)) return true;
        }
        return false;
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
        if (!(o instanceof LKQLList other)) return false;
        return this.internalEquals(other);
    }
}
