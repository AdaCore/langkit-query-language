//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import java.util.HashMap;
import java.util.Map;

/** This class represents the base of all "dynamic object"-like values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLDynamicObject extends DynamicObject {

    /** Uncached library usable for uncached operations */
    protected static final DynamicObjectLibrary uncachedObjectLibrary =
        DynamicObjectLibrary.getUncached();

    // ----- Constructors -----

    public LKQLDynamicObject(Shape shape) {
        super(shape);
    }

    // ----- Uncached access methods -----

    /**
     * Get the value in the LKQL object at the given key with the uncached strategy. If the key
     * doesn't exist then a null value is returned. This method uses an uncached library so this is
     * not design for performance critical usages.
     */
    public Object getUncached(final Object key) {
        return uncachedObjectLibrary.getOrDefault(this, key, null);
    }

    /**
     * Get the key array from the object value. This method uses an uncached library so this is no
     * designed for performance critical usages.
     */
    public Object[] keysUncached() {
        return uncachedObjectLibrary.getKeyArray(this);
    }

    /** Get a map containing all keys in this object value, associated with their values. */
    public Map<String, Object> asMap() {
        Object[] keys = this.keysUncached();
        var values = new HashMap<String, Object>();
        for (int i = 0; i < keys.length; i++) {
            values.put((String) keys[i], this.getUncached(keys[i]));
        }
        return values;
    }

    // ----- Class methods -----

    /** Compare two "object-like" LKQL values using provided libraries to access them. */
    public static boolean compareWithLib(
        LKQLDynamicObject left,
        LKQLDynamicObject right,
        DynamicObjectLibrary leftLib,
        DynamicObjectLibrary rightLib
    ) {
        // Get the objects key sets and compare their size
        Object[] leftKeys = leftLib.getKeyArray(left);
        Object[] rightKeys = rightLib.getKeyArray(right);
        if (leftKeys.length != rightKeys.length) return false;

        // Then compare each value
        for (Object key : leftKeys) {
            if (!rightLib.containsKey(right, key)) return false;
            if (
                !eq(leftLib.getOrDefault(left, key, null), rightLib.getOrDefault(right, key, null))
            ) return false;
        }

        // If we get here, the objects are equals
        return true;
    }

    /** Util function to compare values in Truffle inspectable code. */
    @CompilerDirectives.TruffleBoundary
    private static boolean eq(Object left, Object right) {
        return left.equals(right);
    }

    // ----- Value methods -----

    /**
     * Get the default displayable string, exporting because this message is abstract and all
     * classes which export the interop library must implement it.
     */
    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") boolean allowSideEffects) {
        return "<object_lkql_value>";
    }

    /** Tell the interop library that this value has members. */
    @ExportMessage
    public boolean hasMembers() {
        return true;
    }

    /**
     * Get if the given member is in the receiver object like value. All members are readable but
     * not modifiable.
     */
    @ExportMessage
    public boolean isMemberReadable(
        String member,
        @CachedLibrary("this") DynamicObjectLibrary objectLibrary
    ) {
        return objectLibrary.containsKey(this, member);
    }

    /** Get the existing members for the receiver object like value. */
    @ExportMessage
    public Object getMembers(
        @SuppressWarnings("unused") boolean includeInternal,
        @CachedLibrary("this") DynamicObjectLibrary objectLibrary
    ) {
        return objectLibrary.getKeyArray(this);
    }

    /** Get the value of the wanted member in the receiver object like value. */
    @ExportMessage
    public Object readMember(
        String member,
        @CachedLibrary("this") DynamicObjectLibrary objectLibrary
    ) throws UnknownIdentifierException {
        final Object result = objectLibrary.getOrDefault(this, member, null);
        if (result == null) throw UnknownIdentifierException.create(member);
        return result;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return (String) InteropLibrary.getUncached().toDisplayString(this);
    }

    /** Compare two "object-like" LKQL values, this function only compare keys and their values. */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LKQLDynamicObject other)) return false;
        return compareWithLib(
            this,
            other,
            DynamicObjectLibrary.getUncached(),
            DynamicObjectLibrary.getUncached()
        );
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(this);
    }
}
