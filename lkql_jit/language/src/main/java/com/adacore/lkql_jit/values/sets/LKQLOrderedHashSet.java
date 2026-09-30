//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.sets;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.values.interop.LKQLIterator;
import com.adacore.lkql_jit.values.interop.LKQLValue;
import com.adacore.lkql_jit.values.interop.Utils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.LinkedHashSet;

/** This class represents a set in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public final class LKQLOrderedHashSet extends LKQLValue {

    // ----- Attributes -----

    public final LinkedHashSet<Object> internalSet;

    // ----- Constructors -----

    /** Create a new empty set. */
    public LKQLOrderedHashSet() {
        this.internalSet = new LinkedHashSet<>();
    }

    @SuppressWarnings("unchecked")
    public LKQLOrderedHashSet(LKQLOrderedHashSet other) {
        this.internalSet = (LinkedHashSet<Object>) other.internalSet.clone();
    }

    // TODO
    public LKQLIterator iterator() {
        return null;
    }

    // ----- Value methods -----

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public Object toDisplayString(
        @SuppressWarnings("unused") final boolean allowSideEffect,
        @CachedLibrary(
            limit = Constants.DISPATCHED_LIB_LIMIT
        ) @Cached.Exclusive InteropLibrary elems
    ) {
        if (internalSet.isEmpty()) return "{}";

        // Prepare the result
        final var builder = new StringBuilder("{");

        final var last = internalSet.getLast();

        // Iterate over the list values
        for (var elem : internalSet) {
            // Get the element string
            String elemString;
            if (elem instanceof String) {
                elemString = Utils.toRepr((String) elems.toDisplayString(elem));
            } else {
                elemString = (String) elems.toDisplayString(elem);
            }

            // Add the element string to the result
            builder.append(elemString);
            if (elem != last) builder.append(", ");
        }

        // Return the result
        builder.append("}");
        return builder.toString();
    }

    @CompilerDirectives.TruffleBoundary
    public boolean equals(Object o) {
        if (o instanceof LKQLOrderedHashSet other) return internalSet.equals(other.internalSet);
        return false;
    }

    @CompilerDirectives.TruffleBoundary
    public int hashCode() {
        return internalSet.hashCode();
    }
}
