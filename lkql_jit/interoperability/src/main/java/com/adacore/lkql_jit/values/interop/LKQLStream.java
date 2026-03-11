//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import com.adacore.lkql_jit.values.interfaces.Indexable;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents the base of collection like values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLStream extends LKQLValue implements Iterable, Indexable {

    public abstract Object getHead();

    public abstract LKQLStream getTail();

    // ----- Value methods -----

    /** Get the displayable string for the interop library. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffect) {
        return "<Stream>";
    }

    /** Tell the interop API that the list can be cast to a boolean. */
    @ExportMessage
    public boolean isBoolean() {
        return true;
    }

    /** Get the boolean representation of the list. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public boolean asBoolean() {
        try {
            this.get(0);
            return true;
        } catch (IndexOutOfBoundsException _) {
            return false;
        }
    }

    /** Tell the interop library that the list has an iterator object. */
    @ExportMessage
    public boolean hasIterator() {
        return true;
    }

    /** Get the iterator for the list. */
    @ExportMessage
    public Object getIterator() {
        return this.iterator();
    }
}
