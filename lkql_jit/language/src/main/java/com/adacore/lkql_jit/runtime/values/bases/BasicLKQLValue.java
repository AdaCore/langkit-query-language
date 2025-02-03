//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.bases;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class is the base for all "basic" LKQL values. */
@ExportLibrary(InteropLibrary.class)
public abstract class BasicLKQLValue implements LKQLValue {

    // ----- Attributes -----

    protected final InteropLibrary thisUncachedLibrary = InteropLibrary.getUncached(this);

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
        return "<basic_lkql_value>";
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return (String) thisUncachedLibrary.toDisplayString(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof BasicLKQLValue other)) return false;
        InteropLibrary otherUncached = InteropLibrary.getUncached(other);
        return thisUncachedLibrary.isIdentical(this, other, otherUncached);
    }

    @Override
    public int hashCode() {
        try {
            return thisUncachedLibrary.identityHashCode(this);
        } catch (UnsupportedMessageException e) {
            throw LKQLRuntimeException.shouldNotHappen(
                "All LKQL values must export an 'identityHashCode' message"
            );
        }
    }
}
