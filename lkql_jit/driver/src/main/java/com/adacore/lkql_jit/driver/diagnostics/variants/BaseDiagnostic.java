//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.diagnostics.Hint;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/** This class is the base of all diagnostics. */
public abstract sealed class BaseDiagnostic permits Warning, Error, RuleViolation, RawMessage {

    // ----- Attribbutes -----

    /** Message of the diagnostic. */
    public final String message;

    /** Source section this diagnostic is about. */
    public final Optional<SourceSection> location;

    /** List of hints for this diagnostic. */
    public final List<Hint> hints;

    // ----- Constructors -----

    protected BaseDiagnostic(String message, Optional<SourceSection> location) {
        this.message = message;
        this.location = location;
        this.hints = new ArrayList<>();
    }

    // ----- Instance methods -----

    public BaseDiagnostic withHint(String message) {
        this.hints.add(new Hint(message));
        return this;
    }

    public BaseDiagnostic withHint(String message, SourceSection location) {
        this.hints.add(new Hint(message, location));
        return this;
    }

    @Override
    public String toString() {
        return (
            this.getClass().getSimpleName() + "(message=" + message + ", location=" + location + ")"
        );
    }
}
