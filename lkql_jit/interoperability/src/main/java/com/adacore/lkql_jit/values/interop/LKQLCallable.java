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
import com.oracle.truffle.api.nodes.Node;

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
    public final CallableKind kind;

    /** Names of parameters of this callable value. */
    public final String[] parameterNames;

    /**
     * Truffle nodes representing default values for this callable parameters. This array may
     * contain null nodes.
     */
    public final Node[] parameterDefaultValues;

    /** User documentation for the callable value. */
    public final String documentation;

    // ----- Constructors -----

    protected LKQLCallable(
        String name,
        CallableKind kind,
        String[] parameterNames,
        Node[] parameterDefaultValues,
        String documentation
    ) {
        this.name = name;
        this.kind = kind;
        this.parameterNames = parameterNames;
        this.parameterDefaultValues = parameterDefaultValues;
        this.documentation = documentation;
    }

    // ----- Instance methods -----

    /** Get a string representation of this callable profile. */
    @CompilerDirectives.TruffleBoundary
    public String profile() {
        var expandedParams = new String[this.parameterNames.length];
        for (int i = 0; i < parameterNames.length; i++) {
            var defVal = parameterDefaultValues[i];
            if (defVal != null) {
                expandedParams[i] =
                    parameterNames[i] + "=" + defVal.getSourceSection().getCharacters().toString();
            } else {
                expandedParams[i] = parameterNames[i];
            }
        }
        return (this.name + "(" + String.join(", ", expandedParams) + ")");
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
