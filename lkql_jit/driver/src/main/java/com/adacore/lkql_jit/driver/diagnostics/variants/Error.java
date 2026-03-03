//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/** This class represents an error diagnostic. */
public final class Error extends BaseDiagnostic {

    /** Call stack of the error diagnostic, most recent call first. */
    public final List<CallStackElement> callStack;

    // ----- Constructors -----

    /** Create a new error diagnostic with a related location. */
    public Error(String message, SourceSection location) {
        super(message, Optional.ofNullable(location));
        this.callStack = new ArrayList<>();
    }

    /** Create a new error diagnostic without a location. */
    public Error(String message) {
        this(message, null);
    }

    // ----- Instance methods -----

    /** Add a call at the end of the call stack of this error. */
    public void addCallToStack(String callContext, SourceSection callLocation) {
        this.callStack.add(new CallStackElement(callContext, callLocation));
    }

    // ----- Inner classes -----

    /**
     * This class represents an abstract element of an error call stack.
     *
     * @param callContext Name of the context the call has been done (wrapping function, script
     *                    file name, ...)
     * @param callLocation Where the call has been made.
     */
    public record CallStackElement(String callContext, SourceSection callLocation) {}
}
