//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.checker;

import com.adacore.lkql_jit.runtime.values.LKQLFunction;

/**
 * This class represents a checker in the LKQL language. A checker is an annotated function which is
 * going to be used to perform the node and unit checking.
 */
public abstract class BaseChecker {

    // ----- Attributes -----

    /** Name of the checker (This is the rule name). */
    protected final String name;

    /** Alias of the checker (User can define alias to rules). */
    protected String alias;

    /** Function to execute as checker. */
    protected final LKQLFunction function;

    /** Function to call to auto-fix an entity violating the check. */
    public final LKQLFunction autoFix;

    /** Message to display when the rule violated. */
    protected final String message;

    /** Help to display when rule violated. */
    protected final String help;

    /** Whether the generic instantiations in the checker. */
    protected final boolean followGenericInstantiations;

    /** Category of the checker. */
    protected final String category;

    /** Sub-category of the checker. */
    protected final String subcategory;

    /** Remediation level of the checker. */
    protected final Remediation remediation;

    /** Execution cost of the checker. */
    protected final long executionCost;

    /** If the checker has a parametric exemption. */
    protected final boolean parametricExemption;

    /** Version impacted by the checker. */
    protected final String impact;

    /** Target of the checker. */
    protected final String target;

    // ----- Constructors -----

    /** Create a base checker with all its value. */
    public BaseChecker(
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
            final String impact,
            final String target) {
        this.name = name;
        this.function = function;
        this.autoFix = autoFix;
        this.message = message;
        this.help = help;
        this.followGenericInstantiations = followGenericInstantiations;
        this.category = category;
        this.subcategory = subcategory;
        this.remediation = remediation;
        this.executionCost = executionCost;
        this.parametricExemption = parametricExemption;
        this.impact = impact;
        this.target = target;
    }

    // ----- Getters -----

    public String getName() {
        return name;
    }

    public String getAlias() {
        return alias;
    }

    public LKQLFunction getFunction() {
        return function;
    }

    public String getMessage() {
        return message;
    }

    public boolean isFollowGenericInstantiations() {
        return followGenericInstantiations;
    }

    // ----- Setters -----

    public void setAlias(String alias) {
        this.alias = alias;
    }

    // ----- Instance methods -----

    /** Create a deep copy of the current checker and return it. */
    public abstract BaseChecker copy();

    // ----- Inner classes -----

    /** This enum represents the valid remediation levels */
    public enum Remediation {
        EASY,
        MEDIUM,
        MAJOR
    }
}
