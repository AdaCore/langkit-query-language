/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.utils.checkers;

import com.adacore.lkql_jit.runtime.values.FunctionValue;

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
    protected final FunctionValue function;

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
            final FunctionValue function,
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

    public FunctionValue getFunction() {
        return function;
    }

    public String getMessage() {
        return message;
    }

    public String getHelp() {
        return help;
    }

    public boolean isFollowGenericInstantiations() {
        return followGenericInstantiations;
    }

    public String getCategory() {
        return category;
    }

    public String getSubcategory() {
        return subcategory;
    }

    public Remediation getRemediation() {
        return remediation;
    }

    public long getExecutionCost() {
        return executionCost;
    }

    public boolean isParametricExemption() {
        return parametricExemption;
    }

    public String getImpact() {
        return impact;
    }

    public String getTarget() {
        return target;
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
