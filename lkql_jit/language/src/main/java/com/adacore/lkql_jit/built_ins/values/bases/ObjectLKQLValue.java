//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.values.bases;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;

/** This class is the base for all LKQL object-like values. */
@ExportLibrary(InteropLibrary.class)
public abstract class ObjectLKQLValue extends DynamicObject implements LKQLValue {

    protected static final DynamicObjectLibrary uncachedObjectLibrary =
            DynamicObjectLibrary.getUncached();
    ;

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

    /**
     * Internal method to perform equality checking on object like LKQL values.
     *
     * @param lefts The library to access 'left' messages.
     * @param rights The library to access 'right' messages.
     * @param leftValues Interop library to access the 'left' values messages.
     * @param rightValues Interop library to access the 'right' values messages.
     */
    protected static boolean objectValueEquals(
            ObjectLKQLValue left,
            ObjectLKQLValue right,
            DynamicObjectLibrary lefts,
            DynamicObjectLibrary rights,
            InteropLibrary leftValues,
            InteropLibrary rightValues) {
        // Get the objects key sets and compare their size
        Object[] leftKeys = lefts.getKeyArray(left);
        Object[] rightKeys = rights.getKeyArray(right);
        if (leftKeys.length != rightKeys.length) return false;

        // Then compare each value
        for (Object key : leftKeys) {
            if (!rights.containsKey(right, key)) return false;
            Object leftValue = lefts.getOrDefault(left, key, null);
            Object rightValue = rights.getOrDefault(right, key, null);
            if (leftValues.hasIdentity(leftValue)) {
                if (!leftValues.isIdentical(leftValue, rightValue, rightValues)) return false;
            } else {
                if (!ObjectUtils.equals(leftValue, rightValue)) return false;
            }
        }

        // If we get here, the objects are equals
        return true;
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
            String member, @CachedLibrary("this") DynamicObjectLibrary objectLibrary) {
        return objectLibrary.containsKey(this, member);
    }

    /** Get the existing members for the receiver object like value. */
    @ExportMessage
    public Object getMembers(
            @SuppressWarnings("unused") boolean includeInternal,
            @CachedLibrary("this") DynamicObjectLibrary objectLibrary) {
        return new LKQLList(objectLibrary.getKeyArray(this));
    }

    /** Get the value of the wanted member in the receiver object like value. */
    @ExportMessage
    public Object readMember(
            String member, @CachedLibrary("this") DynamicObjectLibrary objectLibrary)
            throws UnknownIdentifierException {
        final Object result = objectLibrary.getOrDefault(this, member, null);
        if (result == null) throw UnknownIdentifierException.create(member);
        return result;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        InteropLibrary thisUncached = InteropLibrary.getUncached(this);
        return (String) thisUncached.toDisplayString(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof BasicLKQLValue other)) return false;
        InteropLibrary thisUncached = InteropLibrary.getUncached(this);
        InteropLibrary otherUncached = InteropLibrary.getUncached(other);
        return thisUncached.isIdentical(this, other, otherUncached);
    }

    @Override
    public int hashCode() {
        try {
            InteropLibrary thisUncached = InteropLibrary.getUncached(this);
            return thisUncached.identityHashCode(this);
        } catch (UnsupportedMessageException e) {
            throw LKQLRuntimeException.shouldNotHappen(
                    "All LKQL values must export an 'identityHashCode' message");
        }
    }
}
