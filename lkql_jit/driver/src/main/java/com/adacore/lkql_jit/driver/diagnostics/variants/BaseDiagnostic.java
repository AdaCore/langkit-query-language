//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.diagnostics.AutoFix;
import com.adacore.lkql_jit.driver.diagnostics.Hint;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/** This class is the base of all diagnostics. */
public abstract sealed class BaseDiagnostic permits Info, Warning, Error, Exception, RuleViolation {

    // ----- Attribbutes -----

    /** Message of the diagnostic. */
    public final String message;

    /** Source section this diagnostic is about. */
    public final Optional<SourceSection> location;

    /** List of hints for this diagnostic. */
    public final List<Hint> hints;

    /** Auto-fixes resolving this rule violation. */
    public final List<AutoFix> autoFixes;

    // ----- Constructors -----

    protected BaseDiagnostic(String message, Optional<SourceSection> location) {
        this.message = message;
        this.location = location;
        this.hints = new ArrayList<>();
        this.autoFixes = new ArrayList<>();
    }

    // ----- Instance methods -----

    public void addHint(Hint hint) {
        this.hints.add(hint);
    }

    public void addAutoFix(AutoFix autoFix) {
        this.autoFixes.add(autoFix);
    }

    @Override
    public String toString() {
        return (
            this.getClass().getSimpleName() + "(message=" + message + ", location=" + location + ")"
        );
    }
}
