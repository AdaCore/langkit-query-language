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
 * @param name The unique name of the rule, used as its identifier.
 * @param checker LKQL callable object representing the rule logic.
 * @param autoFix Optional LKQL callable to fix node flagged by the rule.
 * @param message Message to display to the used when this rule flags a node.
 * @param help Help message associated to the rule.
 * @param followGenericInstantiations Whether the rule should be applied to nodes in generic
 *                                    instantiations.
 * @param category Category of the rule, this is used for documentation purposes.
 * @param subcategory A subcategory used for documentation.
 * @param remediation How hard it is to remediate to violations of this rule.
 * @param executionCost An estimation of the performance cost to execute this rule.
 * @param parametricExemption Whether this rules support parametric exemptions.
 * @param target Which target this rule is aiming for.
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
