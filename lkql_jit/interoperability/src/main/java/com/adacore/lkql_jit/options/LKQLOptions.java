//
//  Copyright (C) 2005-2026, AdaCore
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
 * If you want to create an instance of this class, please use the {@link Builder} builder class.
 *
 * @param engineMode In which mode the engine should run.
 * @param verbose Whether the engine should display additional information about the execution
 *                process.
 * @param checkerDebug Whether the debug mode should be enabled for the checker process.
 * @param diagnosticOutputMode The format diagnostics should be output in.
 * @param charset Charset to use to decode sources.
 * @param files Explicit list of files to analyze.
 * @param ignores Explicit list of files to exclude from the analysis.
 * @param rulesDirs Directories to fetch LKQL rules from.
 * @param ruleInstances All rule instances to execute during the checking process.
 * @param fallbackToAllRules If no instance have been provided, whether to execute all known rules.
 * @param missingFileIsError If a file is missing from the analysis, whether to consider this event
 *                           as an error.
 * @param showInstantiationChain Show instantiation chains when a rule violation is raised inside an
 *                               Ada generic instantiation.
 * @param additionalProjectPaths Directories to search GPR projects in.
 * @param autoconf Autoconf option to forward to GPR2.
 * @param configFile Config file to forward to GPR2.
 * @param additionalKnowledgeBases Additional Ada knowledge bases.
 * @param skipStandardKnowledgeBase Do not process the standard Ada knowledge base.
 * @param implicitWiths Projects to consider as "withed" by analyzed sources.
 * @param followSymlinks Whether to follow symbolic links when loading the GPR project.
 * @param noProject If true, do not try to load a GPR project.
 * @param projectFile The GPR project file to load.
 * @param subprojectFile Name of the subproject to analyze.
 * @param rootDir Root directory to forward to GPR2.
 * @param relocateBuildTree Relocate build tree to forward to GPR2.
 * @param adaRuntime Runtime to use to analyze Ada sources.
 * @param runtimes Additional runtimes.
 * @param srcSubdirs Source subdirs to forward to GPR2.
 * @param subdirs Subdirs to forward to GPR2.
 * @param target Target to use when loading a GPR project.
 * @param scenarioVariables Scenario variables to forward to GPR2 when loading a project.
 */
public record LKQLOptions(
    // LKQL specific options
    EngineMode engineMode,
    boolean verbose,
    boolean checkerDebug,
    DiagnosticOutputMode diagnosticOutputMode,
    Optional<String> charset,
    List<String> files,
    List<String> ignores,
    List<String> rulesDirs,
    Map<String, RuleInstance> ruleInstances,
    boolean fallbackToAllRules,
    boolean missingFileIsError,
    boolean showInstantiationChain,

    // GPR options
    List<String> additionalProjectPaths,
    Optional<String> autoconf,
    Optional<String> configFile,
    List<String> additionalKnowledgeBases,
    boolean skipStandardKnowledgeBase,
    List<String> implicitWiths,
    boolean followSymlinks,
    boolean noProject,
    Optional<String> projectFile,
    Optional<String> subprojectFile,
    Optional<String> rootDir,
    Optional<String> relocateBuildTree,
    Optional<String> adaRuntime,
    Map<String, String> runtimes,
    Optional<String> srcSubdirs,
    Optional<String> subdirs,
    Optional<String> target,
    Map<String, String> scenarioVariables
) {
    // ----- Constructors -----

    public LKQLOptions {
        // Ensure that there is no null values in the LKQL options, also ensure that all
        // contained values are strictly unmodifiable.
        if (engineMode == null) {
            engineMode = EngineMode.INTERPRETER;
        }

        if (diagnosticOutputMode == null) {
            diagnosticOutputMode = DiagnosticOutputMode.PRETTY;
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

        if (additionalProjectPaths == null) {
            additionalProjectPaths = List.of();
        } else {
            additionalProjectPaths = Collections.unmodifiableList(additionalProjectPaths);
        }

        if (additionalKnowledgeBases == null) {
            additionalKnowledgeBases = List.of();
        } else {
            additionalKnowledgeBases = Collections.unmodifiableList(additionalKnowledgeBases);
        }

        if (implicitWiths == null) {
            implicitWiths = List.of();
        } else {
            implicitWiths = Collections.unmodifiableList(implicitWiths);
        }

        if (runtimes == null) {
            runtimes = Map.of();
        } else {
            runtimes = Collections.unmodifiableMap(runtimes);
        }

        if (scenarioVariables == null) {
            scenarioVariables = Map.of();
        } else {
            scenarioVariables = Collections.unmodifiableMap(scenarioVariables);
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
            // LKQL specific options
            EngineMode.valueOf(jsonLKQLOptions.getString("engineMode")),
            jsonLKQLOptions.getBoolean("verbose"),
            jsonLKQLOptions.getBoolean("checkerDebug"),
            DiagnosticOutputMode.valueOf(jsonLKQLOptions.getString("diagnosticOutputMode")),
            Optional.ofNullable(jsonLKQLOptions.optString("charset", null)),
            jsonLKQLOptions
                .getJSONArray("files")
                .toList()
                .stream()
                .map(e -> (String) e)
                .toList(),
            jsonLKQLOptions
                .getJSONArray("ignores")
                .toList()
                .stream()
                .map(e -> (String) e)
                .toList(),
            jsonLKQLOptions
                .getJSONArray("rulesDirs")
                .toList()
                .stream()
                .map(e -> (String) e)
                .toList(),
            ruleInstances,
            jsonLKQLOptions.getBoolean("fallbackToAllRules"),
            jsonLKQLOptions.getBoolean("missingFileIsError"),
            jsonLKQLOptions.getBoolean("showInstantiationChain"),
            // GPR options
            jsonLKQLOptions
                .getJSONArray("additionalProjectPaths")
                .toList()
                .stream()
                .map(e -> (String) e)
                .toList(),
            Optional.ofNullable(jsonLKQLOptions.optString("autoconf", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("configFile", null)),
            jsonLKQLOptions
                .getJSONArray("additionalKnowledgeBases")
                .toList()
                .stream()
                .map(e -> (String) e)
                .toList(),
            jsonLKQLOptions.getBoolean("skipStandardKnowledgeBase"),
            jsonLKQLOptions
                .getJSONArray("implicitWiths")
                .toList()
                .stream()
                .map(e -> (String) e)
                .toList(),
            jsonLKQLOptions.getBoolean("followSymlinks"),
            jsonLKQLOptions.getBoolean("noProject"),
            Optional.ofNullable(jsonLKQLOptions.optString("projectFile", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("subprojectFile", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("rootDir", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("relocateBuildTree", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("adaRuntime", null)),
            JSONUtils.parseStringMap(jsonLKQLOptions.getJSONObject("runtimes")),
            Optional.ofNullable(jsonLKQLOptions.optString("srcSubdirs", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("subdirs", null)),
            Optional.ofNullable(jsonLKQLOptions.optString("target", null)),
            JSONUtils.parseStringMap(jsonLKQLOptions.getJSONObject("scenarioVariables"))
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
            .put("checkerDebug", checkerDebug)
            .put("diagnosticOutputMode", diagnosticOutputMode.toString())
            .put("charset", charset.orElse(null))
            .put("files", new JSONArray(files))
            .put("ignores", new JSONArray(ignores))
            .put("rulesDirs", new JSONArray(rulesDirs))
            .put("ruleInstances", ruleInstancesJson)
            .put("fallbackToAllRules", fallbackToAllRules)
            .put("missingFileIsError", missingFileIsError)
            .put("showInstantiationChain", showInstantiationChain)
            .put("additionalProjectPaths", new JSONArray(additionalProjectPaths))
            .put("autoconf", autoconf.orElse(null))
            .put("configFile", configFile.orElse(null))
            .put("additionalKnowledgeBases", new JSONArray(additionalKnowledgeBases))
            .put("skipStandardKnowledgeBase", skipStandardKnowledgeBase)
            .put("implicitWiths", new JSONArray(implicitWiths))
            .put("followSymlinks", followSymlinks)
            .put("noProject", noProject)
            .put("projectFile", projectFile.orElse(null))
            .put("subprojectFile", subprojectFile.orElse(null))
            .put("rootDir", rootDir.orElse(null))
            .put("relocateBuildTree", relocateBuildTree.orElse(null))
            .put("adaRuntime", adaRuntime.orElse(null))
            .put("runtimes", new JSONObject(runtimes))
            .put("srcSubdirs", srcSubdirs.orElse(null))
            .put("subdirs", subdirs.orElse(null))
            .put("target", target.orElse(null))
            .put("scenarioVariables", new JSONObject(scenarioVariables));
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

    /** Represents the mode the LKQL engine runs on. */
    public enum EngineMode {
        /** LKQL engine is just going to run the provided LKQL script, without doing more. */
        INTERPRETER,

        /** LKQL engine will seek for defined rules and make them accessible. */
        CHECKER,
    }

    /** Util class to build a new LKQL options object. */
    public static final class Builder {

        // ----- Options -----

        // LKQL specific options
        private EngineMode engineMode = EngineMode.INTERPRETER;
        private boolean verbose = false;
        private boolean checkerDebug = false;
        private DiagnosticOutputMode diagnosticOutputMode = DiagnosticOutputMode.PRETTY;
        private Optional<String> charset = Optional.empty();
        private List<String> files = new ArrayList<>();
        private List<String> ignores = new ArrayList<>();
        private List<String> rulesDirs = new ArrayList<>();
        private Map<String, RuleInstance> ruleInstances = new HashMap<>();
        private boolean fallbackToAllRules = false;
        private boolean missingFileIsError = false;
        private boolean showInstantiationChain = false;

        // GPR options
        private List<String> additionalProjectPaths = new ArrayList<>();
        private Optional<String> autoconf = Optional.empty();
        private Optional<String> configFile = Optional.empty();
        private List<String> additionalKnowledgeBases = new ArrayList<>();
        private boolean skipStandardKnowledgeBase = false;
        private List<String> implicitWiths = new ArrayList<>();
        private boolean followSymlinks = false;
        private boolean noProject = false;
        private Optional<String> projectFile = Optional.empty();
        private Optional<String> subprojectFile = Optional.empty();
        private Optional<String> rootDir = Optional.empty();
        private Optional<String> relocateBuildTree = Optional.empty();
        private Optional<String> adaRuntime = Optional.empty();
        private Map<String, String> runtimes = new HashMap<>();
        private Optional<String> srcSubdirs = Optional.empty();
        private Optional<String> subdirs = Optional.empty();
        private Optional<String> target = Optional.empty();
        private Map<String, String> scenarioVariables = new HashMap<>();

        // ----- Setters -----

        // --- LKQL specific options

        public Builder engineMode(EngineMode em) {
            engineMode = em;
            return this;
        }

        public Builder verbose(boolean v) {
            verbose = v;
            return this;
        }

        public Builder checkerDebug(boolean cd) {
            checkerDebug = cd;
            return this;
        }

        public Builder diagnosticOutputMode(DiagnosticOutputMode dom) {
            diagnosticOutputMode = dom;
            return this;
        }

        public Builder charset(String c) {
            charset = Optional.ofNullable(c);
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

        public Builder fallbackToAllRules(boolean fbtar) {
            fallbackToAllRules = fbtar;
            return this;
        }

        public Builder missingFileIsError(boolean mfie) {
            missingFileIsError = mfie;
            return this;
        }

        public Builder showInstantiationChain(boolean sic) {
            showInstantiationChain = sic;
            return this;
        }

        // --- GPR options

        public Builder additionalProjectPaths(List<String> app) {
            additionalProjectPaths = app;
            return this;
        }

        public Builder autoconf(String ac) {
            autoconf = Optional.ofNullable(ac);
            return this;
        }

        public Builder configFile(String cf) {
            configFile = Optional.ofNullable(cf);
            return this;
        }

        public Builder additionalKnowledgeBases(List<String> akb) {
            additionalKnowledgeBases = akb;
            return this;
        }

        public Builder skipStandardKnowledgeBase(boolean sskb) {
            skipStandardKnowledgeBase = sskb;
            return this;
        }

        public Builder implicitWiths(List<String> iw) {
            implicitWiths = iw;
            return this;
        }

        public Builder followSymlinks(boolean fs) {
            followSymlinks = fs;
            return this;
        }

        public Builder noProject(boolean np) {
            noProject = np;
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

        public Builder rootDir(String rd) {
            rootDir = Optional.ofNullable(rd);
            return this;
        }

        public Builder relocateBuildTree(String rbt) {
            relocateBuildTree = Optional.ofNullable(rbt);
            return this;
        }

        public Builder adaRuntime(String ar) {
            adaRuntime = Optional.ofNullable(ar);
            return this;
        }

        public Builder runtimes(Map<String, String> r) {
            runtimes = r;
            return this;
        }

        public Builder srcSubdirs(String ss) {
            srcSubdirs = Optional.ofNullable(ss);
            return this;
        }

        public Builder subdirs(String s) {
            subdirs = Optional.ofNullable(s);
            return this;
        }

        public Builder target(String t) {
            target = Optional.ofNullable(t);
            return this;
        }

        public Builder scenarioVariables(Map<String, String> sv) {
            scenarioVariables = sv;
            return this;
        }

        // ----- Instance methods -----

        public LKQLOptions build() {
            return new LKQLOptions(
                engineMode,
                verbose,
                checkerDebug,
                diagnosticOutputMode,
                charset,
                files,
                ignores,
                rulesDirs,
                ruleInstances,
                fallbackToAllRules,
                missingFileIsError,
                showInstantiationChain,
                additionalProjectPaths,
                autoconf,
                configFile,
                additionalKnowledgeBases,
                skipStandardKnowledgeBase,
                implicitWiths,
                followSymlinks,
                noProject,
                projectFile,
                subprojectFile,
                rootDir,
                relocateBuildTree,
                adaRuntime,
                runtimes,
                srcSubdirs,
                subdirs,
                target,
                scenarioVariables
            );
        }
    }
}
