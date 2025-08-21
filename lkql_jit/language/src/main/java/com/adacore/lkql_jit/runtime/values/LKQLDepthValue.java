//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/**
 * This class represents an LKQL Value with the depth information.
 */
@ExportLibrary(InteropLibrary.class)
@CompilerDirectives.ValueType
public final class LKQLDepthValue extends BasicLKQLValue {

    // ----- Attributes -----

    /** The depth of the node. */
    public final int depth;

    /** The decorated node. */
    public final Object value;

    // ----- Constructors -----

    /** Create a new depth node. */
    public LKQLDepthValue(int depth, Object value) {
        this.depth = depth;
        this.value = value;
        assert !(value instanceof LKQLDepthValue);
    }

    /** Exported message to compare two LKQL depth nodes. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {

        /** Compare two LKQL depth nodes. */
        @Specialization
        @CompilerDirectives.TruffleBoundary
        protected static TriState onNode(final LKQLDepthValue left, final LKQLDepthValue right) {
            return TriState.valueOf(left.value.equals(right.value));
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
            @SuppressWarnings("unused") final LKQLDepthValue receiver,
            @SuppressWarnings("unused") final Object other
        ) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL depth node. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public static int identityHashCode(final LKQLDepthValue receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return this.toString();
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return "<DepthVal " + this.value.toString() + ">";
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public int hashCode() {
        return this.value.hashCode();
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof LKQLDepthValue other)) return false;
        return this.value.equals(other.value);
    }
}
