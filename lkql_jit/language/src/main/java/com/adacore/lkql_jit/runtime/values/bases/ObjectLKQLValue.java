//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.bases;

import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;

/** This class is the base for all LKQL object-like values. */
@ExportLibrary(InteropLibrary.class)
public abstract class ObjectLKQLValue extends DynamicObject {

    protected static final DynamicObjectLibrary uncachedObjectLibrary =
        DynamicObjectLibrary.getUncached();

    // ----- Constructors -----

    public ObjectLKQLValue(Shape shape) {
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

    // ----- Class methods -----

    /** Compare two "object-like" LKQL values using provided libraries to access them. */
    public static boolean compareWithLib(
        ObjectLKQLValue left,
        ObjectLKQLValue right,
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
                !ObjectUtils.equals(
                    leftLib.getOrDefault(left, key, null),
                    rightLib.getOrDefault(right, key, null)
                )
            ) return false;
        }

        // If we get here, the objects are equals
        return true;
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
        return new LKQLList(objectLibrary.getKeyArray(this));
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
        if (!(o instanceof ObjectLKQLValue other)) return false;
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
