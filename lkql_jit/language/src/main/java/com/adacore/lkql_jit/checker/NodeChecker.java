//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.checker;

import com.adacore.lkql_jit.runtime.values.LKQLFunction;

/** This class represents a node checker in the LKQL system. */
public final class NodeChecker extends BaseChecker {

    // ----- Constructors -----

    /** Create a new node checker. */
    public NodeChecker(
        final String name,
        final LKQLFunction function,
        final LKQLFunction autoFix,
        final String message,
        final String help,
        final boolean followGenericInstantiations,
        final String category,
        final String subcategory,
        final Remediation remediation,
        final long executionCost,
        final boolean parametricExemption,
        final String target
    ) {
        super(
            name,
            function,
            autoFix,
            message,
            help,
            followGenericInstantiations,
            category,
            subcategory,
            remediation,
            executionCost,
            parametricExemption,
            target
        );
    }

    // ----- Instance methods -----

    @Override
    public BaseChecker copy() {
        return new NodeChecker(
            this.name,
            this.function,
            this.autoFix,
            this.message,
            this.help,
            this.followGenericInstantiations,
            this.category,
            this.subcategory,
            this.remediation,
            this.executionCost,
            this.parametricExemption,
            this.target
        );
    }
}
