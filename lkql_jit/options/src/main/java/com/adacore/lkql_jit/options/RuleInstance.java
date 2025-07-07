//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options;

import java.util.Map;
import java.util.Optional;
import org.graalvm.polyglot.SourceSection;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * This record represents a rule instantiation made by the user through the command-line. This
 * record is used to abstract all rule configuration formats. It can be serialized and deserialized
 * using the JSON format. A rule instance JSON object looks like this:
 *
 * <pre>
 * {
 *     "ruleName": "rule_1"  // Lower-cased name of the rule which is instantiated
 *     "instanceName": null  // User defined name of the instance, can be null or string
 *     "sourceMode": "ADA"   // The instance source mode as a string
 *     "arguments": {}       // An object created from the 'arguments' field
 * }
 * </pre>
 *
 * @param ruleName Lower-cased name of the rule this instantiation comes from.
 * @param instanceName Optional user-defined name of this instance, casing is stored.
 * @param sourceMode Mode of the instance regarding the Ada sources.
 * @param arguments Named arguments to pass to the LKQL function representing this rule, the
 *     argument value is an LKQL expression.
 */
public record RuleInstance(
    String ruleName,
    Optional<String> instanceName,
    SourceMode sourceMode,
    Map<String, String> arguments,
    SourceSection instanceLocation
) {
    // ----- Constructor -----

    public RuleInstance {
        // Rule instance cannot have an instance name equals to its rule name
        if (instanceName.isPresent() && instanceName.get().toLowerCase().equals(ruleName)) {
            instanceName = Optional.empty();
        }

        // If the provided argument map is null, then set it to an empty immutable map
        if (arguments == null) {
            arguments = Map.of();
        }
    }

    // ----- Constructors -----

    public static RuleInstance fromJson(JSONObject jsonObject) throws JSONException {
        return new RuleInstance(
            jsonObject.getString("ruleName"),
            Optional.ofNullable(jsonObject.optString("instanceName", null)),
            SourceMode.valueOf(jsonObject.getString("sourceMode")),
            JSONUtils.parseStringMap(jsonObject.getJSONObject("arguments")),
            null
        );
    }

    // ----- Instance methods -----

    /**
     * Get the unique identifier of this instance. An instance is identified by its name, the user
     * defined instance name if any, the rule name otherwise.
     */
    public String instanceId() {
        return this.instanceName.orElse(this.ruleName).toLowerCase();
    }

    public JSONObject toJson() {
        return new JSONObject()
            .put("ruleName", this.ruleName)
            .put("instanceName", this.instanceName.orElse(null))
            .put("sourceMode", this.sourceMode.toString())
            .put("arguments", new JSONObject(this.arguments));
        // Do not output source location information for now
    }

    /** Return whether this instance is equivalent to the other one, i.e.: it
     * runs the same checker with the same parameters (only the instance name
     * can differ). */
    public boolean isEquivalent(RuleInstance o) {
        return (
            this.ruleName.equals(o.ruleName) &&
            this.sourceMode.equals(o.sourceMode) &&
            this.arguments.equals(o.arguments)
        );
    }

    /** Return a String holding the location of the instance in the GNAT
     * Diagnosis format when possible. */
    public String locationToGNATDiagnosisFormatString() {
        if (instanceLocation != null) {
            return (
                instanceLocation.getSource().getName() +
                ":" +
                instanceLocation.getStartLine() +
                ":" +
                instanceLocation.getStartColumn()
            );
        } else {
            return "undefined:0:0";
        }
    }

    // ----- Override methods ------

    @Override
    public int hashCode() {
        return this.instanceId().hashCode();
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof RuleInstance other)) return false;
        return this.instanceId().equals(other.instanceId());
    }

    // ----- Inner classes -----

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
