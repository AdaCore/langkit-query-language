//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.checker;

import com.adacore.lkql_jit.driver.source_support.SourceSection;
import com.adacore.lkql_jit.values.interop.LKQLNoValue;
import java.util.Map;
import java.util.Optional;

/** This record represents an instantiated rule. */
public final class RuleInstance {

    // ----- Attributes -----

    /** Which rule is instantiated. */
    public final Rule instantiatedRule;

    /** A custom name for this instance. */
    public final Optional<String> instanceName;

    /** To which kind of source this instance should apply to. */
    public final SourceMode sourceMode;

    /** Named arguments for the rule function. */
    public final Map<String, Object> arguments;

    /**
     * Object array that is going to be used to call the rule function, pre-filled with instance
     * arguments.
     */
    public final Object[] checkerArguments;

    /**
     * Object array that is going to be used to call the rule auto fixing function, pre-filled with
     * the function closure if there is some.
     * If the instantiated rule hasn't any auto-fix function, this attribute is `null`.
     */
    public final Object[] autoFixArguments;

    /** Where in source this instance has been created. */
    public final Optional<SourceSection> instanceLocation;

    // ----- Constructors -----

    public RuleInstance(
        Rule instantiatedRule,
        Optional<String> instanceName,
        SourceMode sourceMode,
        Map<String, Object> arguments,
        Optional<SourceSection> instanceLocation
    ) {
        this.instantiatedRule = instantiatedRule;
        this.instanceName = instanceName;
        this.sourceMode = sourceMode;
        this.arguments = arguments;
        this.instanceLocation = instanceLocation;

        // Fill checker call arguments
        var paramNames = instantiatedRule.checker().parameterNames;
        var closureOffset = 0;
        if (instantiatedRule.checker().takesClosure()) {
            this.checkerArguments = new Object[paramNames.length + 1];
            this.checkerArguments[0] = this.instantiatedRule.checker().getClosure();
            closureOffset = 1;
        } else {
            this.checkerArguments = new Object[paramNames.length];
        }
        for (int i = 1; i < paramNames.length; i++) {
            var arg = arguments.get(paramNames[i]);
            if (arg != null) {
                checkerArguments[i + closureOffset] = arg;
            } else {
                checkerArguments[i + closureOffset] = LKQLNoValue.INSTANCE;
            }
        }

        // Fill auto fixing call arguments if there is an auto-fix function
        if (instantiatedRule.autoFix().isPresent()) {
            var autoFix = instantiatedRule.autoFix().get();
            if (autoFix.takesClosure()) {
                this.autoFixArguments = new Object[autoFix.parameterNames.length + 1];
                this.autoFixArguments[0] = autoFix.getClosure();
            } else {
                this.autoFixArguments = new Object[autoFix.parameterNames.length];
            }
        } else {
            this.autoFixArguments = null;
        }
    }

    // ----- Instance methods -----

    @Override
    public String toString() {
        return (
            "RuleInstance(" +
            "instantiatedRule=" +
            instantiatedRule +
            ", instanceName=" +
            instanceName +
            ", sourceMode=" +
            sourceMode +
            ", arguments=" +
            arguments +
            ")"
        );
    }

    // ----- Inner enums -----

    /** This enum represents the mode of an instance about the Ada sources. */
    public enum SourceMode {
        /** The instance will be executed on all Ada sources. */
        GENERAL,

        /** The instance will only be executed on pure Ada code (non-SPARK). */
        ADA,

        /** The instance will be executed on SPARK code only. */
        SPARK,
    }
}
