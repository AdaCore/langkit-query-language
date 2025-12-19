//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/**
 * This class represents an abstraction over all LKQL callable values. To call such values, you
 * should use the "execute" message from the 'InteropLibrary'.
 */
@ExportLibrary(InteropLibrary.class)
public abstract class LKQLCallable extends LKQLValue {

    // ----- Attributes -----

    /** Name of the callable value. */
    public final String name;

    /** Kind of the callable value. */
    private final CallableKind kind;

    /** Names of parameters of this callable value. */
    public final String[] parameterNames;

    /** User documentation for the callable value. */
    public final String documentation;

    // ----- Constructors -----

    protected LKQLCallable(
        String name,
        CallableKind kind,
        String[] parameterNames,
        String documentation
    ) {
        this.name = name;
        this.kind = kind;
        this.parameterNames = parameterNames;
        this.documentation = documentation;
    }

    // ----- Value methods -----

    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return this.kind.toString().toLowerCase() + "<" + this.name + ">";
    }

    @ExportMessage
    public boolean isExecutable() {
        return true;
    }

    /**
     * The default behavior of the execution interop message is to crash. Concrete implementations
     * of this class will define their own behavior.
     */
    @ExportMessage
    public Object execute(Object[] arguments)
        throws UnsupportedTypeException, ArityException, UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    public boolean hasExecutableName() {
        return true;
    }

    @ExportMessage
    public String getExecutableName() {
        return this.name;
    }

    // ----- Inner classes and enums -----

    /** represents the kind of a callable value. */
    public static enum CallableKind {
        FUNCTION,
        SELECTOR,
        PROPERTY,
    }
}
