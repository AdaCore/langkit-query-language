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

package com.adacore.lkql_jit.built_ins.values;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;
import java.util.Arrays;
import java.util.Objects;

/** This class represents a tuple in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLTuple implements TruffleObject, Indexable {

    // ----- Attributes -----

    /** The content of the tuple. */
    private final Object[] content;

    // ----- Constructors -----

    /** Create a new tuple with its content. */
    public LKQLTuple(final Object[] content) {
        this.content = content;
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

    /** Exported message to compare two tuples. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {
        /** Compare two LKQL tuples. */
        @Specialization
        public static TriState onTuple(final LKQLTuple left, final LKQLTuple right) {
            if (left.internalEquals(right)) return TriState.TRUE;
            else return TriState.FALSE;
        }

        /** Do the comparison with another element. */
        @Fallback
        public static TriState onOther(
                @SuppressWarnings("unused") final LKQLTuple receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL tuple. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(final LKQLTuple receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public Object toDisplayString(
            @SuppressWarnings("unused") final boolean allowSideEffect,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary interopLibrary) {
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

    /** Tell the interop library that the value is array like. */
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
    public Object get(int index) throws InvalidIndexException {
        try {
            return this.content[index];
        } catch (IndexOutOfBoundsException e) {
            throw new InvalidIndexException();
        }
    }

    @Override
    public Object[] getContent() {
        return this.content;
    }

    // ----- LKQL value methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean internalEquals(final LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof LKQLTuple other)) return false;
        if (other.content.length != this.content.length) return false;
        for (int i = 0; i < this.content.length; i++) {
            Object mineObject = this.content[i];
            Object hisObject = other.content[i];
            if ((mineObject instanceof LKQLValue mine) && (hisObject instanceof LKQLValue his)) {
                if (!mine.internalEquals(his)) return false;
            } else {
                if (!Objects.equals(mineObject, hisObject)) return false;
            }
        }
        return true;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        InteropLibrary tupleLib = InteropLibrary.getUncached(this);
        return (String) tupleLib.toDisplayString(this);
    }

    @Override
    public boolean equals(final Object o) {
        if (o == this) return true;
        if (!(o instanceof LKQLTuple other)) return false;
        return this.internalEquals(other);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(this.content);
    }
}
