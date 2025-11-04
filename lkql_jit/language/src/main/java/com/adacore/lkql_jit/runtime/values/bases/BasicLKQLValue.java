//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.bases;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class is the base for all "basic" LKQL values. */
@ExportLibrary(InteropLibrary.class)
public abstract class BasicLKQLValue implements TruffleObject {

    // ----- Value methods -----

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
        return (String) InteropLibrary.getUncached().toDisplayString(this);
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof BasicLKQLValue other)) return false;
        return InteropLibrary.getUncached(this).isIdentical(
            this,
            other,
            InteropLibrary.getUncached(other)
        );
    }
}
