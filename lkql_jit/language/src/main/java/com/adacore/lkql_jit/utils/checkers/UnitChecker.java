//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.checkers;

import com.adacore.lkql_jit.built_ins.values.LKQLFunction;

/** This class represents a unit checker in the LKQL system. */
public final class UnitChecker extends BaseChecker {

    // ----- Constructors -----

    /** Create a new unit checker. */
    public UnitChecker(
            final String name,
            final LKQLFunction function,
            final String message,
            final String help,
            final boolean followGenericInstantiations,
            final String category,
            final String subcategory,
            final Remediation remediation,
            final long executionCost,
            final boolean parametricExemption,
            final String impact,
            final String target) {
        super(
                name,
                function,
                message,
                help,
                followGenericInstantiations,
                category,
                subcategory,
                remediation,
                executionCost,
                parametricExemption,
                impact,
                target);
    }

    // ----- Instance methods -----

    @Override
    public BaseChecker copy() {
        return new UnitChecker(
                this.name,
                this.function,
                this.message,
                this.help,
                this.followGenericInstantiations,
                this.category,
                this.subcategory,
                this.remediation,
                this.executionCost,
                this.parametricExemption,
                this.impact,
                this.target);
    }
}
