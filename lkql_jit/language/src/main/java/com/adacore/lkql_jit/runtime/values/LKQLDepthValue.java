//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

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

    /** Get the displayable string for the interop library. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return "<DepthVal " + this.value.toString() + ">";
    }

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof LKQLDepthValue other)) return false;
        return ObjectUtils.equals(this.value, other.value);
    }

    @Override
    public int hashCode() {
        return ObjectUtils.hashCode(this.value);
    }
}
