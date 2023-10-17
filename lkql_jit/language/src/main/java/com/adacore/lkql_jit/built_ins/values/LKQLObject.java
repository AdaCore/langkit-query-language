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
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLArrayList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import com.oracle.truffle.api.utilities.TriState;
import java.util.Arrays;
import java.util.Objects;

/** This class represents an object value in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public final class LKQLObject extends DynamicObject implements TruffleObject, LKQLValue {

    // ----- Class attributes -----

    /** An uncached dynamic object library. */
    private static final DynamicObjectLibrary objectLibrary = DynamicObjectLibrary.getUncached();

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
            objectLibrary.put(res, keys[i], values[i]);
        }
        return res;
    }

    // ----- Instance methods ------

    /**
     * Get the value in the LKQL object at the given key with the uncached strategy. If the key
     * doesn't exist then a null value is returned. This method uses an uncached library so this is
     * not design for performance critical usages.
     */
    public Object getUncached(final Object key) {
        return objectLibrary.getOrDefault(this, key, null);
    }

    /**
     * Get the key array from the object value. This method uses an uncached library so this is no
     * designed for performance critical usages.
     */
    public Object[] keysUncached() {
        return objectLibrary.getKeyArray(this);
    }

    // ----- Value methods -----

    /** Tell the interop API that the value has an associated language. */
    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    /** Give the LKQL language class to the interop library. */
    @ExportMessage
    Class<? extends TruffleLanguage<?>> getLanguage() {
        return LKQLLanguage.class;
    }

    /** Exported message to compare two LKQL objects. */
    @ExportMessage
    static class IsIdenticalOrUndefined {
        /** Compare two LKQL objects. */
        @Specialization
        protected static TriState onLKQLObject(final LKQLObject left, final LKQLObject right) {
            if (left.internalEquals(right)) return TriState.TRUE;
            else return TriState.FALSE;
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
                @SuppressWarnings("unused") final LKQLObject receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL object. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    static int identityHashCode(final LKQLObject receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string of the object. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object toDisplayString(
            @SuppressWarnings("unused") final boolean allowSideEffects,
            @CachedLibrary("this") DynamicObjectLibrary thisLibrary,
            @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) InteropLibrary interopLibrary) {
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

    /** Tell the interop library that the value has members. */
    @ExportMessage
    boolean hasMembers() {
        return true;
    }

    /**
     * Get if the given member is in the receiver object. All members are readable but not
     * modifiable.
     */
    @ExportMessage
    boolean isMemberReadable(
            final String member, @CachedLibrary("this") DynamicObjectLibrary objectLibrary) {
        return objectLibrary.containsKey(this, member);
    }

    /** Get the existing members for the receiver object. */
    @ExportMessage
    Object getMembers(
            @SuppressWarnings("unused") final boolean includeInternal,
            @CachedLibrary("this") DynamicObjectLibrary objectLibrary) {
        return new LKQLArrayList(objectLibrary.getKeyArray(this));
    }

    /** Get the value of the wanted member in the receiver object. */
    @ExportMessage
    Object readMember(
            final String member, @CachedLibrary("this") DynamicObjectLibrary objectLibrary)
            throws UnknownIdentifierException {
        Object result = objectLibrary.getOrDefault(this, member, null);
        if (result == null) throw UnknownIdentifierException.create(member);
        return result;
    }

    // ----- LKQL value methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean internalEquals(final LKQLValue o) {
        if (this == o) return true;
        if (!(o instanceof LKQLObject other)) return false;

        // Create the libraries to access object fields
        DynamicObjectLibrary thisLib = DynamicObjectLibrary.getFactory().getUncached(this);
        DynamicObjectLibrary otherLib = DynamicObjectLibrary.getFactory().getUncached(other);

        // Get the keys
        Object[] thisKeys = thisLib.getKeyArray(this);
        Object[] otherKeys = otherLib.getKeyArray(other);

        // Check the key array length
        if (thisKeys.length != otherKeys.length) return false;

        // Iterate over the keys and verify their values
        for (Object key : thisKeys) {
            // Ensure that the other contains the key
            if (!otherLib.containsKey(other, key)) return false;

            // Compare the values
            Object thisObject = thisLib.getOrDefault(this, key, null);
            Object otherObject = otherLib.getOrDefault(other, key, null);
            if ((thisObject instanceof LKQLValue thisValue)
                    && (otherObject instanceof LKQLValue otherValue)) {
                if (!thisValue.internalEquals(otherValue)) return false;
            } else {
                if (!Objects.equals(thisObject, otherObject)) return false;
            }
        }

        // Return the success
        return true;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        InteropLibrary objectLib = InteropLibrary.getUncached(this);
        return (String) objectLib.toDisplayString(this);
    }

    @Override
    public boolean equals(final Object o) {
        if (o == this) return true;
        if (!(o instanceof LKQLObject other)) return false;
        return this.internalEquals(other);
    }

    @Override
    public int hashCode() {
        Object[] keys = this.keysUncached();
        Object[] values = new Object[keys.length];
        for (int i = 0; i < keys.length; i++) {
            values[i] = this.getUncached(keys[i]);
        }
        return Arrays.hashCode(ArrayUtils.concat(keys, values));
    }
}
