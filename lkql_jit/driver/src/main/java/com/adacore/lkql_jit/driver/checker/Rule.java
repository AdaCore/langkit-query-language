//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.checker;

import com.adacore.lkql_jit.values.interop.LKQLCallable;
import java.util.Optional;

/**
 * Represents an LKQL rule, extracted from an LKQL file and defined by a checking function.
 *
 * @param kind Which is the kind of this rule, what is its calling convention.
 */
public record Rule(
    Kind kind,
    String name,
    LKQLCallable checker,
    Optional<LKQLCallable> autoFix,
    String message,
    String help,
    boolean followGenericInstantiations,
    String category,
    String subcategory,
    Remediation remediation,
    long executionCost,
    boolean parametricExemption,
    String target
) {
    // ----- Inner enums -----

    /** Kind of a rule, which input it should be called with. */
    public enum Kind {
        NODE,
        UNIT,
    }

    /** Rule remediation estimated complexity. */
    public enum Remediation {
        EASY,
        MEDIUM,
        MAJOR,
    }
}
