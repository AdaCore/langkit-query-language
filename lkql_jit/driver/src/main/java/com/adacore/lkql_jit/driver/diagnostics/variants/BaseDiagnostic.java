//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.ListComparator;
import com.adacore.lkql_jit.driver.diagnostics.AutoFix;
import com.adacore.lkql_jit.driver.diagnostics.Hint;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.*;
import java.util.function.Function;

/** This class is the base of all diagnostics. */
public abstract sealed class BaseDiagnostic
    implements Comparable<BaseDiagnostic>
    permits Info, Warning, Error, Exception, RuleViolation {

    // ----- Attribbutes -----

    /** Message of the diagnostic. */
    public final String message;

    /** Source section this diagnostic is about. */
    public final Optional<SourceSection> location;

    /** List of hints for this diagnostic. */
    public final List<Hint> hints;

    /** Auto-fix resolving this diagnostic, if any. */
    public Optional<AutoFix> autoFix;

    // ----- Constructors -----

    protected BaseDiagnostic(String message, Optional<SourceSection> location) {
        this.message = message;
        this.location = location;
        this.hints = new ArrayList<>();
        this.autoFix = Optional.empty();
    }

    // ----- Instance methods -----

    public void addHint(Hint hint) {
        this.hints.add(hint);
    }

    public void setAutoFix(AutoFix autoFix) {
        this.autoFix = Optional.of(autoFix);
    }

    @Override
    public String toString() {
        return (
            this.getClass().getSimpleName() + "(message=" + message + ", location=" + location + ")"
        );
    }

    @Override
    public int compareTo(BaseDiagnostic other) {
        // Compare the location if both diagnostics have one
        if (location.isPresent() && other.location.isEmpty()) return 1;
        else if (location.isEmpty() && other.location.isPresent()) return -1;
        if (location.isPresent()) {
            var locationCompare = location.get().compareTo(other.location.get());
            if (locationCompare != 0) return locationCompare;
        }

        // Then, compare messages and hints
        var messageAndHintsCompare = Comparator.comparing((BaseDiagnostic d) -> d.message)
            .thenComparing(d -> d.hints, new ListComparator<>(Hint::compareTo))
            .compare(this, other);
        if (messageAndHintsCompare != 0) {
            return messageAndHintsCompare;
        }

        // Now compare auto-fixes
        if (autoFix.isPresent() && other.autoFix.isEmpty()) return -1;
        else if (autoFix.isEmpty() && other.autoFix.isPresent()) return 1;
        if (autoFix.isPresent()) {
            var autoFixCompare = autoFix.get().compareTo(other.autoFix.get());
            if (autoFixCompare != 0) return autoFixCompare;
        }

        // Finally, compare diagnostic kinds
        Function<BaseDiagnostic, Integer> kindOrder = d ->
            switch (d) {
                case RuleViolation _ -> 1;
                case Exception _ -> 2;
                case Error _ -> 3;
                case Warning _ -> 4;
                case Info _ -> 5;
            };
        return kindOrder.apply(this).compareTo(kindOrder.apply(other));
    }

    @Override
    public boolean equals(Object o) {
        if (o == null) return false;
        if (!(o instanceof BaseDiagnostic other)) return false;
        return compareTo(other) == 0;
    }
}
