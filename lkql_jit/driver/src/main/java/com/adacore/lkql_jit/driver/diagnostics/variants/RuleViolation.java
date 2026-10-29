//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.ListComparator;
import com.adacore.lkql_jit.driver.checker.RuleInstance;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/** This class represents a rule violation diagnostics. */
public final class RuleViolation extends BaseDiagnostic {

    // ----- Attributes -----

    /** Rule instance that has been violated. */
    public final RuleInstance violatedInstance;

    /**
     * If the violation comes from an instantiated node, this contains the trace of where this
     * instantiation comes from.
     */
    public final List<SourceSection> genericTrace;

    // ----- Constructors -----

    /** Create a new rule violation diagnostic with a generic trace. */
    public RuleViolation(
        String message,
        RuleInstance violatedInstance,
        SourceSection location,
        List<SourceSection> genericTrace
    ) {
        // This isn't possible to provide no location for a rule violation
        super(message, Optional.of(location));
        this.violatedInstance = violatedInstance;
        this.genericTrace = genericTrace;
    }

    /** Create a new rule violation diagnostic with the message of the violated instance. */
    public RuleViolation(
        RuleInstance violatedInstance,
        SourceSection location,
        List<SourceSection> genericTrace
    ) {
        this(violatedInstance.instantiatedRule.message(), violatedInstance, location, genericTrace);
    }

    // ----- Instance methods -----

    @Override
    public int compareTo(BaseDiagnostic other) {
        var superRes = super.compareTo(other);
        if (other instanceof RuleViolation otherViolation && superRes == 0) {
            return Comparator.comparing((RuleViolation v) -> v.violatedInstance.identifier())
                .thenComparing(v -> v.genericTrace, new ListComparator<>(SourceSection::compareTo))
                .compare(this, otherViolation);
        } else {
            return superRes;
        }
    }
}
