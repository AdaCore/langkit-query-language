//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options;

import java.util.*;
import java.util.stream.Collectors;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * This record contains all options to tune the LKQL engine. It is serializable and deserializable
 * using the JSON format.
 */
public record LKQLOptions(
    EngineMode engineMode,
    boolean verbose,
    Optional<String> charset,
    Optional<String> projectFile,
    Optional<String> subprojectFile,
    Optional<String> runtime,
    Optional<String> target,
    Optional<String> configFile,
    Map<String, String> scenarioVariables,
    List<String> files,
    List<String> ignores,
    List<String> rulesDirs,
    Map<String, RuleInstance> ruleInstances,
    boolean checkerDebug,
    boolean fallbackToAllRules,
    boolean keepGoingOnMissingFile,
    boolean showInstantiationChain,
    DiagnosticOutputMode diagnosticOutputMode,
    AutoFixMode autoFixMode
) {
    // ----- Constructors -----

    public LKQLOptions {
        // Ensure that there is no null values in the LKQL options, also ensure that all
        // contained values are strictly unmodifiable.
        if (engineMode == null) {
            engineMode = EngineMode.INTERPRETER;
        }

        if (scenarioVariables == null) {
            scenarioVariables = Map.of();
        } else {
            scenarioVariables = Collections.unmodifiableMap(scenarioVariables);
        }

        if (files == null) {
            files = List.of();
        } else {
            files = Collections.unmodifiableList(files);
        }

        if (ignores == null) {
            ignores = List.of();
        } else {
            ignores = Collections.unmodifiableList(ignores);
        }

        if (rulesDirs == null) {
            rulesDirs = List.of();
        } else {
            rulesDirs = Collections.unmodifiableList(rulesDirs);
        }

        if (ruleInstances == null) {
            ruleInstances = Map.of();
        } else {
            ruleInstances = Collections.unmodifiableMap(ruleInstances);
        }

        if (diagnosticOutputMode == null) {
            diagnosticOutputMode = DiagnosticOutputMode.PRETTY;
        }

        if (autoFixMode == null) {
            autoFixMode = AutoFixMode.DISPLAY;
        }
    }

    // ----- Class methods -----

    /** Create an LKQL options object from the provided JSON object. */
    public static LKQLOptions fromJson(JSONObject jsonLKQLOptions) {
        final Map<String, RuleInstance> ruleInstances = new HashMap<>();
        final var ruleInstancesJson = jsonLKQLOptions.getJSONObject("ruleInstances");
        for (String instanceName : ruleInstancesJson.keySet()) {
            ruleInstances.put(
                instanceName,
                RuleInstance.fromJson(ruleInstancesJson.getJSONObject(instanceName))
            );
        }
        return new LKQLOptions(
            EngineMode.valueOf(jsonLKQLOptions.getString("engineMode")),
            jsonLKQLOptions.getBoolean("verbose"),
            Optional.ofNullable(jsonLKQLOptions.optString("charset", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("projectFile", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("subprojectFile", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("runtime", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("target", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("configFile", null)),
            JSONUtils.parseStringMap(jsonLKQLOptions.getJSONObject("scenarioVariables")),
            jsonLKQLOptions.getJSONArray("files").toList().stream().map(e -> (String) e).toList(),
            jsonLKQLOptions.getJSONArray("ignores").toList().stream().map(e -> (String) e).toList(),
            jsonLKQLOptions
                .getJSONArray("rulesDirs")
                .toList()
                .stream()
                .map(e -> (String) e)
                .toList(),
            ruleInstances,
            jsonLKQLOptions.getBoolean("checkerDebug"),
            jsonLKQLOptions.getBoolean("fallbackToAllRules"),
            jsonLKQLOptions.getBoolean("keepGoingOnMissingFile"),
            jsonLKQLOptions.getBoolean("showInstantiationChain"),
            DiagnosticOutputMode.valueOf(jsonLKQLOptions.getString("diagnosticOutputMode")),
            AutoFixMode.valueOf(jsonLKQLOptions.getString("autoFixMode"))
        );
    }

    /** Get a default options instance. */
    public static LKQLOptions getDefault() {
        return new Builder().build();
    }

    // ----- Instance methods -----

    /** Serialize the LKQL options to a JSON object. */
    public JSONObject toJson() {
        final var ruleInstancesJson = new JSONObject(
            this.ruleInstances.entrySet()
                .stream()
                .map(e -> Map.entry(e.getKey(), e.getValue().toJson()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue))
        );
        return new JSONObject()
            .put("engineMode", engineMode.toString())
            .put("verbose", verbose)
            .put("charset", charset.orElse(null))
            .put("projectFile", projectFile.orElse(null))
            .put("subprojectFile", subprojectFile.orElse(null))
            .put("runtime", runtime.orElse(null))
            .put("target", target.orElse(null))
            .put("configFile", configFile.orElse(null))
            .put("scenarioVariables", new JSONObject(scenarioVariables))
            .put("files", new JSONArray(files))
            .put("ignores", new JSONArray(ignores))
            .put("rulesDirs", new JSONArray(rulesDirs))
            .put("ruleInstances", ruleInstancesJson)
            .put("checkerDebug", checkerDebug)
            .put("fallbackToAllRules", fallbackToAllRules)
            .put("keepGoingOnMissingFile", keepGoingOnMissingFile)
            .put("showInstantiationChain", showInstantiationChain)
            .put("diagnosticOutputMode", diagnosticOutputMode.toString())
            .put("autoFixMode", autoFixMode.toString());
    }

    // ----- Inner classes -----

    /** The way diagnostics are output by the LKQL engine. */
    public enum DiagnosticOutputMode {
        /**
         * Emit a pretty diagnostic with source listing where the diagnostic location is
         * highlighted.
         */
        PRETTY,

        /** Use a GNATCheck-compliant format: "{file}:{line}:{col} check: {message} [{check}]". */
        GNATCHECK,
    }

    /** This enum represents the mode for the auto fixes application. */
    public enum AutoFixMode {
        /** Display the patched analysis unit to stdout. */
        DISPLAY,

        /** Create a new file alongside the original one, containing the patched analysis unit. */
        NEW_FILE,

        /** Replace the content of the original file with the patched analysis unit. */
        PATCH_FILE,
    }

    /** Represents the mode the LKQL engine runs on. */
    public enum EngineMode {
        /** LKQL engine is just going to run the provided LKQL script, without doing more. */
        INTERPRETER,

        /** LKQL engine will seek for defined rules and make them accessible. */
        CHECKER,

        /**
         * LKQL engine will seek for rules and check that they all define an auto-fixing function.
         */
        FIXER,
    }

    /** Util class to build a new LKQL options object. */
    public static final class Builder {

        // ----- Options -----

        private EngineMode engineMode = EngineMode.INTERPRETER;
        private boolean verbose = false;
        private Optional<String> charset = Optional.empty();
        private Optional<String> projectFile = Optional.empty();
        private Optional<String> subprojectFile = Optional.empty();
        private Optional<String> runtime = Optional.empty();
        private Optional<String> target = Optional.empty();
        private Optional<String> configFile = Optional.empty();
        private Map<String, String> scenarioVariables = new HashMap<>();
        private List<String> files = new ArrayList<>();
        private List<String> ignores = new ArrayList<>();
        private List<String> rulesDirs = new ArrayList<>();
        private Map<String, RuleInstance> ruleInstances = new HashMap<>();
        private boolean checkerDebug = false;
        private boolean fallbackToAllRules = false;
        private boolean keepGoingOnMissingFile = false;
        private boolean showInstantiationChain = false;
        private DiagnosticOutputMode diagnosticOutputMode = DiagnosticOutputMode.PRETTY;
        private AutoFixMode autoFixMode = AutoFixMode.DISPLAY;

        // ----- Setters -----

        public Builder engineMode(EngineMode em) {
            engineMode = em;
            return this;
        }

        public Builder verbose(boolean v) {
            verbose = v;
            return this;
        }

        public Builder charset(String c) {
            charset = Optional.ofNullable(c);
            return this;
        }

        public Builder projectFile(String pf) {
            projectFile = Optional.ofNullable(pf);
            return this;
        }

        public Builder subprojectFile(String spf) {
            subprojectFile = Optional.ofNullable((spf));
            return this;
        }

        public Builder runtime(String r) {
            runtime = Optional.ofNullable(r);
            return this;
        }

        public Builder target(String t) {
            target = Optional.ofNullable(t);
            return this;
        }

        public Builder configFile(String cf) {
            configFile = Optional.ofNullable(cf);
            return this;
        }

        public Builder scenarioVariables(Map<String, String> sv) {
            scenarioVariables = sv;
            return this;
        }

        public Builder files(List<String> f) {
            files = f;
            return this;
        }

        public Builder addFile(String f) {
            files.add(f);
            return this;
        }

        public Builder ignores(List<String> i) {
            ignores = i;
            return this;
        }

        public Builder rulesDir(List<String> rd) {
            rulesDirs = rd;
            return this;
        }

        public Builder ruleInstances(Map<String, RuleInstance> ri) {
            ruleInstances = ri;
            return this;
        }

        public Builder checkerDebug(boolean cd) {
            checkerDebug = cd;
            return this;
        }

        public Builder fallbackToAllRules(boolean fbtar) {
            fallbackToAllRules = fbtar;
            return this;
        }

        public Builder keepGoingOnMissingFile(boolean kgomf) {
            keepGoingOnMissingFile = kgomf;
            return this;
        }

        public Builder showInstantiationChain(boolean sic) {
            showInstantiationChain = sic;
            return this;
        }

        public Builder diagnosticOutputMode(DiagnosticOutputMode dom) {
            diagnosticOutputMode = dom;
            return this;
        }

        public Builder autoFixMode(AutoFixMode afm) {
            autoFixMode = afm;
            return this;
        }

        // ----- Instance methods -----

        public LKQLOptions build() {
            return new LKQLOptions(
                engineMode,
                verbose,
                charset,
                projectFile,
                subprojectFile,
                runtime,
                target,
                configFile,
                scenarioVariables,
                files,
                ignores,
                rulesDirs,
                ruleInstances,
                checkerDebug,
                fallbackToAllRules,
                keepGoingOnMissingFile,
                showInstantiationChain,
                diagnosticOutputMode,
                autoFixMode
            );
        }
    }
}
