//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.libadalang.Libadalang;
import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.checker.*;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.options.JsonUtils;
import com.adacore.lkql_jit.options.RuleInstance;
import com.adacore.lkql_jit.runtime.CallStack;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.LalLocationWrapper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.util.*;
import java.util.function.BiConsumer;

/**
 * This class represents the execution context of an LKQL script.
 *
 * @author Hugo GUERRIER
 */
public final class LKQLContext {
    private final LKQLLanguage language;

    // ----- Attributes -----

    /** Environment of the language. */
    @CompilerDirectives.CompilationFinal private TruffleLanguage.Env env;

    /** The global values of the LKQL execution. */
    private final GlobalScope global;

    public final CheckerUtils.SourceLinesCache linesCache = new CheckerUtils.SourceLinesCache();

    /** The call stack of the current language thread. */
    public final CallStack callStack = new CallStack();

    // ----- Ada project attributes -----

    /** The analysis context for the ada files. */
    private Libadalang.AnalysisContext adaContext;

    /** The project manager for the ada project. */
    private Libadalang.ProjectManager projectManager;

    /** Event handler for the project manager. */
    private final Libadalang.EventHandler eventHandler =
            Libadalang.EventHandler.create(
                    (ctx, name, from, found, notFoundIsError) -> {
                        if (!found && notFoundIsError) {
                            boolean isFatal = !this.keepGoingOnMissingFile();
                            this.getDiagnosticEmitter()
                                    .emitFileNotFound(
                                            new LalLocationWrapper(from.getRoot(), this.linesCache),
                                            name,
                                            isFatal);
                            if (isFatal) {
                                this.env.getContext().closeExited(null, 1);
                            }
                        }
                    },
                    null);

    /**
     * The user-specified source files to analyze. If not explicitly specified, those will be the
     * source files of the root project.
     */
    private List<String> specifiedSourceFiles;

    /**
     * All the source files of the project, including those of its non-externally-built
     * dependencies.
     */
    private List<String> allSourceFiles;

    /** Whether the source files were parsed. */
    private boolean parsed;

    /**
     * The user-specified units to analyze. If not explicitly specified, those will be the units of
     * the root project.
     */
    private Libadalang.AnalysisUnit[] specifiedUnits;

    /** All the units of the project, including those of its non-externally-built dependencies. */
    private Libadalang.AnalysisUnit[] allUnits;

    /** The root nodes of all the analysis units of the project. */
    private Libadalang.AdaNode[] allUnitsRoots;

    // ----- Checker attributes -----

    /**
     * All rule instantiated by the user through the command-line. Mapped from their identifier to
     * their value.
     */
    private Map<String, RuleInstance> ruleInstances = null;

    /** A cache for all rule arguments to avoid evaluating twice the same argument source. */
    private Map<String, Map<String, Object>> instancesArgsCache = new HashMap<>();

    /** Whether there is at least one rule that needs to follow generic instantiations. */
    private boolean needsToFollowInstantiations = false;

    /** Node checkers to run on all nodes from the Ada sources. */
    private NodeChecker[] filteredGeneralNodeCheckers = null;

    /** Node checkers to run on non-SPARK nodes from the Ada sources. */
    private NodeChecker[] filteredAdaNodeCheckers = null;

    /** Node checkers to run only on SPARK nodes from the Ada sources. */
    private NodeChecker[] filteredSparkNodeCheckers = null;

    /** Unit checkers to run. */
    private UnitChecker[] filteredUnitCheckers = null;

    // ----- Option caches -----

    /** Whether the language is in the verbose mode. */
    @CompilerDirectives.CompilationFinal private Boolean isVerbose = null;

    @CompilerDirectives.CompilationFinal private Boolean keepGoingOnMissingFile = null;

    /** The project file to analyse. */
    @CompilerDirectives.CompilationFinal private String projectFile = null;

    @CompilerDirectives.CompilationFinal private String target = null;

    @CompilerDirectives.CompilationFinal private String runtime = null;

    @CompilerDirectives.CompilationFinal private String configFile = null;

    /** The project's scenario variables. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private Libadalang.ScenarioVariable[] scenarioVars = null;

    /** The ada files passed through the command line. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] files = null;

    /** Whether the checker is in debug mode. */
    @CompilerDirectives.CompilationFinal private Boolean checkerDebug = null;

    /** The directories where the rule files are located. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] ruleDirectories;

    /** The files to ignore during an analysis. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] ignores;

    /** Tool to emit diagnostics in the wanted format. */
    @CompilerDirectives.CompilationFinal private CheckerUtils.DiagnosticEmitter emitter;

    // ----- Constructors -----

    /**
     * Create a new LKQL context.
     *
     * @param env The environment.
     * @param global The initialized global values.
     */
    public LKQLContext(TruffleLanguage.Env env, GlobalScope global, LKQLLanguage language) {
        this.env = env;
        this.global = global;
        this.specifiedSourceFiles = new ArrayList<>();
        this.allSourceFiles = new ArrayList<>();
        this.parsed = false;
        this.language = language;
    }

    // ----- Destructors -----

    /** Finalize the LKQL context to close libadalang context. */
    public void finalizeContext() {
        this.adaContext.close();
        if (this.projectManager != null) this.projectManager.close();
        this.eventHandler.close();
    }

    // ----- Getters -----

    public TruffleLanguage.Env getEnv() {
        return this.env;
    }

    public GlobalScope getGlobal() {
        return this.global;
    }

    public Libadalang.AnalysisUnit[] getSpecifiedUnits() {
        if (!this.parsed) {
            this.parseSources();
        }
        return this.specifiedUnits;
    }

    public Libadalang.AnalysisUnit[] getAllUnits() {
        if (!this.parsed) {
            this.parseSources();
        }
        return this.allUnits;
    }

    public Libadalang.AdaNode[] getAllUnitsRoots() {
        if (!this.parsed) {
            this.parseSources();
        }
        return this.allUnitsRoots;
    }

    // ----- Setters -----

    public void patchContext(TruffleLanguage.Env newEnv) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        this.env = newEnv;
        this.invalidateOptionCaches();
        this.initSources();
    }

    // ----- Options getting methods -----

    /**
     * Get if the language execution is in verbose mode.
     *
     * @return True if the verbose flag is on.
     */
    public boolean isVerbose() {
        if (this.isVerbose == null) {
            this.isVerbose = this.env.getOptions().get(LKQLLanguage.verbose);
        }
        return this.isVerbose;
    }

    /** Return true if the engine should keep running when a required file is not found. */
    public boolean keepGoingOnMissingFile() {
        if (this.keepGoingOnMissingFile == null) {
            this.keepGoingOnMissingFile =
                    this.env.getOptions().get(LKQLLanguage.keepGoingOnMissingFile);
        }
        return this.keepGoingOnMissingFile;
    }

    /**
     * Return the project file of the language context.
     *
     * @return The project file in a string.
     */
    public String getProjectFile() {
        if (this.projectFile == null) {
            this.projectFile = this.env.getOptions().get(LKQLLanguage.projectFile);
        }
        return this.projectFile;
    }

    public String getTarget() {
        if (this.target == null) {
            this.target = this.env.getOptions().get(LKQLLanguage.target);
        }
        return this.target;
    }

    public String getRuntime() {
        if (this.runtime == null) {
            this.runtime = this.env.getOptions().get(LKQLLanguage.runtime);
        }
        return this.runtime;
    }

    public String getConfigFile() {
        if (this.configFile == null) {
            this.configFile = this.env.getOptions().get(LKQLLanguage.configFile);
        }
        return this.configFile;
    }

    /** Return the list of scenario variables to specify when loading the GPR project file. */
    public Libadalang.ScenarioVariable[] getScenarioVars() {
        if (this.scenarioVars == null) {
            // Scenario variables are passed as semicolon-separated substrings encoded in Base64.
            String[] bindings = this.env.getOptions().get(LKQLLanguage.scenarioVars).split(";");
            if (bindings.length == 1 && bindings[0].length() == 0) {
                // No scenario variables were specified
                this.scenarioVars = new Libadalang.ScenarioVariable[0];
            } else {
                // Some scenario variables were specified. Decode them from Base64 and parse the
                // `key=value`
                // specification.
                Base64.Decoder decoder = Base64.getDecoder();
                this.scenarioVars = new Libadalang.ScenarioVariable[bindings.length];
                for (int i = 0; i < bindings.length; ++i) {
                    String binding = new String(decoder.decode(bindings[i]));
                    int eqIndex = binding.indexOf('=');
                    if (eqIndex == -1) {
                        throw LKQLRuntimeException.fromMessage(
                                "Invalid scenario variable specification: " + binding);
                    }
                    String name = binding.substring(0, eqIndex);
                    String value = binding.substring(eqIndex + 1);
                    this.scenarioVars[i] = Libadalang.ScenarioVariable.create(name, value);
                }
            }
        }
        return this.scenarioVars;
    }

    /**
     * Get the files to analyse.
     *
     * @return The files to analyse in an array.
     */
    public String[] getFiles() {
        if (this.files == null) {
            this.files = StringUtils.splitPaths(this.env.getOptions().get(LKQLLanguage.files));
        }
        return this.files;
    }

    /**
     * Get whether the checker is in debug mode.
     *
     * @return True if the checker is in debug mode, false else
     */
    @CompilerDirectives.TruffleBoundary
    public boolean isCheckerDebug() {
        if (this.checkerDebug == null) {
            this.checkerDebug = this.env.getOptions().get(LKQLLanguage.checkerDebug);
        }
        return this.checkerDebug;
    }

    /**
     * Get the directories to get the rules from.
     *
     * @return The directory array.
     */
    public String[] getRuleDirectories() {
        if (this.ruleDirectories == null) {
            this.ruleDirectories =
                    StringUtils.splitPaths(this.env.getOptions().get(LKQLLanguage.rulesDirs));
            String additionalRulesDirs = System.getenv(Constants.LKQL_RULES_PATH);
            if (additionalRulesDirs != null) {
                this.ruleDirectories =
                        ArrayUtils.concat(
                                this.ruleDirectories, StringUtils.splitPaths(additionalRulesDirs));
            }
        }
        return this.ruleDirectories;
    }

    /**
     * Get the Ada file to ignore during the analysis.
     *
     * @return The array containing all Ada files to ignore.
     */
    public String[] getIgnores() {
        if (this.ignores == null) {
            this.ignores = StringUtils.splitPaths(this.env.getOptions().get(LKQLLanguage.ignores));
            this.ignores =
                    Arrays.stream(this.ignores)
                            .filter(s -> !s.isBlank() && !s.isEmpty())
                            .toArray(String[]::new);
        }
        return this.ignores;
    }

    /** Invalidate the option caches. */
    private void invalidateOptionCaches() {
        this.isVerbose = null;
        this.projectFile = null;
        this.files = null;
        this.ruleInstances = null;
        this.instancesArgsCache = new HashMap<>();
        this.ruleDirectories = null;
        this.ignores = null;
        this.emitter = null;
    }

    // ----- Value related methods -----

    /**
     * Get the meta table for the given type.
     *
     * @param type The type to get the meta table for.
     * @return The meta table for the type.
     */
    public Map<String, BuiltInMethodFactory> getMetaTable(String type) {
        return this.global.getMetaTable(type);
    }

    // ----- IO methods -----

    /**
     * Display the given string.
     *
     * @param toPrint The string to print.
     */
    @CompilerDirectives.TruffleBoundary
    public void print(String toPrint) {
        System.out.print(toPrint);
    }

    /**
     * Display the given string with a newline.
     *
     * @param toPrint The string to print.
     */
    @CompilerDirectives.TruffleBoundary
    public void println(String toPrint) {
        System.out.println(toPrint);
    }

    /**
     * @return the diagnostic emitter to use according to which diagnostic style was chosen.
     */
    @CompilerDirectives.TruffleBoundary
    public CheckerUtils.DiagnosticEmitter getDiagnosticEmitter() {
        if (this.emitter == null) {
            this.emitter =
                    switch (this.env.getOptions().get(LKQLLanguage.diagnosticOutputMode)) {
                        case PRETTY -> new CheckerUtils.DefaultEmitter();
                        case GNATCHECK -> new CheckerUtils.GNATcheckEmitter();
                    };
        }
        return this.emitter;
    }

    // ----- Project analysis methods -----

    /** Parse the ada source files and store analysis units and root nodes. */
    @CompilerDirectives.TruffleBoundary
    public void parseSources() {
        // Filter the Ada source file list
        String[] ignores = this.getIgnores();
        String[] usedSources =
                this.specifiedSourceFiles.stream()
                        .filter(
                                source -> {
                                    for (String ignore : ignores) {
                                        if (source.contains(ignore)) return false;
                                    }
                                    return true;
                                })
                        .toArray(String[]::new);

        // For each specified source file, store its corresponding analysis unit in the list of
        // specified units
        this.specifiedUnits = new Libadalang.AnalysisUnit[usedSources.length];
        for (int i = 0; i < usedSources.length; i++) {
            this.specifiedUnits[i] = this.adaContext.getUnitFromFile(usedSources[i]);
        }

        // For each source file of the project, store its corresponding analysis unit in the list of
        // all
        // the units
        // of the project, as well as their root nodes.
        this.allUnits = new Libadalang.AnalysisUnit[this.allSourceFiles.size()];
        this.allUnitsRoots = new Libadalang.AdaNode[this.allSourceFiles.size()];

        for (int i = 0; i < this.allUnits.length; i++) {
            this.allUnits[i] = this.adaContext.getUnitFromFile(this.allSourceFiles.get(i));
            this.allUnitsRoots[i] = this.allUnits[i].getRoot();
        }

        // All source files are now parsed
        this.parsed = true;
    }

    /** Initialize the ada sources. */
    public void initSources() {
        // Reset the context fields
        this.specifiedSourceFiles.clear();
        this.allSourceFiles.clear();
        this.parsed = false;

        // Add all the user-specified files to process after verifying they exist
        for (String file : this.getFiles()) {
            if (!file.isEmpty() && !file.isBlank()) {
                File sourceFile = new File(file);
                if (sourceFile.isFile()) {
                    this.specifiedSourceFiles.add(sourceFile.getAbsolutePath());
                } else {
                    this.getDiagnosticEmitter()
                            .emitFileNotFound(null, file, !this.keepGoingOnMissingFile());
                }
            }
        }

        // Get the project file and use it if there is one
        final String projectFileName = this.getProjectFile();
        if (!projectFileName.isBlank()) {
            this.projectManager =
                    Libadalang.ProjectManager.create(
                            projectFileName,
                            this.getScenarioVars(),
                            this.getTarget(),
                            this.getRuntime(),
                            this.getConfigFile());

            // Forward the project diagnostics if there are some
            if (!this.projectManager.getDiagnostics().isEmpty()) {
                this.getDiagnosticEmitter()
                        .emitProjectErrors(
                                new File(projectFileName).getName(),
                                this.projectManager.getDiagnostics());
            }

            // Get the subproject provided by the user
            final String subprojectName = this.env.getOptions().get(LKQLLanguage.subprojectFile);
            final String[] subprojects =
                    subprojectName.isEmpty() ? null : new String[] {subprojectName};

            // If no files were specified by the user, the files to analyze are those of the root
            // project (i.e. without recursing into project dependencies)
            if (this.specifiedSourceFiles.isEmpty()) {
                this.specifiedSourceFiles.addAll(
                        Arrays.stream(
                                        this.projectManager.getFiles(
                                                Libadalang.SourceFileMode.ROOT_PROJECT,
                                                subprojects))
                                .toList());
            }

            // The `units()` built-in function must return all units of the project including units
            // from its dependencies. So let's retrieve all those files as well.
            this.allSourceFiles.addAll(
                    Arrays.stream(
                                    this.projectManager.getFiles(
                                            Libadalang.SourceFileMode.WHOLE_PROJECT, subprojects))
                            .toList());

            this.adaContext =
                    this.projectManager.createContext(
                            subprojectName.isEmpty() ? null : subprojectName,
                            this.eventHandler,
                            true,
                            8);
        }

        // Else, either load the implicit project.
        else {
            // We should not get any scenario variable if we are being run without a project file.
            if (this.getScenarioVars().length != 0) {
                throw LKQLRuntimeException.fromMessage(
                        "Scenario variable specifications require a project file");
            }

            // If the option is the empty string, the language implementation will end up setting it
            // to the default value for its language (e.g. iso-8859-1 for Ada).
            String charset = this.env.getOptions().get(LKQLLanguage.charset);

            // Load the implicit project
            this.projectManager =
                    Libadalang.ProjectManager.createImplicit(
                            this.getTarget(), this.getRuntime(), this.getConfigFile());
            this.allSourceFiles.addAll(
                    Arrays.stream(
                                    this.projectManager.getFiles(
                                            Libadalang.SourceFileMode.WHOLE_PROJECT))
                            .toList());
            final Libadalang.UnitProvider provider = this.projectManager.getProvider();

            // Create the ada context and store it in the LKQL context
            this.adaContext =
                    Libadalang.AnalysisContext.create(
                            charset, null, provider, this.eventHandler, true, 8);

            // In the absence of a project file, we consider for now that there are no configuration
            // pragmas.
            this.adaContext.setConfigPragmasMapping(null, null);
        }
    }

    // ----- Checker methods -----

    @CompilerDirectives.TruffleBoundary
    public Map<String, RuleInstance> getRuleInstances() {
        if (this.ruleInstances == null) {
            try {
                this.ruleInstances =
                        JsonUtils.deserializeInstances(
                                this.env.getOptions().get(LKQLLanguage.ruleInstances));
            } catch (Exception e) {
                // Since the LKQL option is internal, the user cannot provide a raw value so this
                // is not supposed to happen.
                e.printStackTrace();
                throw LKQLRuntimeException.shouldNotHappen("Invalid JSON for rule instances");
            }
        }
        return this.ruleInstances;
    }

    /**
     * Get the argument value for the given instance.
     *
     * @param instanceId Identifier of the instance to get the argument from.
     * @param argName Name of the argument to get.
     */
    @CompilerDirectives.TruffleBoundary
    public Object getRuleArg(String instanceId, String argName) {
        final Map<String, Object> instanceArgs =
                this.instancesArgsCache.getOrDefault(instanceId, new HashMap<>());

        // If an argument is not already in the argument values cache, get its source from the
        // registered instances and evaluate it if some.
        if (!instanceArgs.containsKey(argName)) {
            final RuleInstance instance = this.getRuleInstances().get(instanceId);
            final String argSource = instance == null ? null : instance.arguments().get(argName);
            final Object argValue;
            if (argSource == null) {
                argValue = null;
            } else {
                try (var context = Liblkqllang.AnalysisContext.create()) {
                    var unit =
                            context.getUnitFromBuffer(
                                    argSource,
                                    "<rule_arg>",
                                    null,
                                    Liblkqllang.GrammarRule.EXPR_RULE);
                    var root = unit.getRoot();
                    var source =
                            Source.newBuilder(Constants.LKQL_ID, argSource, "<rule_arg>").build();
                    var node = language.translate(root, source);
                    argValue = node.executeGeneric(null);
                } catch (Exception e) {
                    throw LKQLRuntimeException.fromMessage(
                            "Rule argument value generated an "
                                    + "interpreter error: '"
                                    + argSource
                                    + "'");
                }
            }
            instanceArgs.put(argName, argValue);
            this.instancesArgsCache.put(instanceId, instanceArgs);
        }

        return instanceArgs.get(argName);
    }

    /**
     * Get the filtered node rules in this context.
     *
     * @return The node checkers array filtered according to options.
     */
    @CompilerDirectives.TruffleBoundary
    public NodeChecker[] getAllNodeCheckers() {
        if (this.filteredGeneralNodeCheckers == null) {
            this.initCheckerCaches();
        }
        return this.filteredGeneralNodeCheckers;
    }

    /**
     * Get the filtered node checkers for Ada code only.
     *
     * @return The node checkers array for Ada code only.
     */
    public NodeChecker[] getAdaNodeCheckers() {
        if (this.filteredAdaNodeCheckers == null) {
            this.initCheckerCaches();
        }
        return this.filteredAdaNodeCheckers;
    }

    /**
     * Get the filtered node checkers for SPARK code only.
     *
     * @return The node checkers array for SPARK code only.
     */
    public NodeChecker[] getSparkNodeCheckers() {
        if (this.filteredSparkNodeCheckers == null) {
            this.initCheckerCaches();
        }
        return this.filteredSparkNodeCheckers;
    }

    /**
     * Get the filtered unit checkers for the context.
     *
     * @return The list for unit checkers filtered according to options.
     */
    @CompilerDirectives.TruffleBoundary
    public UnitChecker[] getUnitCheckersFiltered() {
        if (this.filteredUnitCheckers == null) {
            this.initCheckerCaches();
        }
        return this.filteredUnitCheckers;
    }

    /** Initialize the filtered and separated checker caches. */
    @CompilerDirectives.TruffleBoundary
    private void initCheckerCaches() {
        // Prepare the working variables
        final List<NodeChecker> generalNodeCheckers = new ArrayList<>();
        final List<NodeChecker> adaNodeCheckers = new ArrayList<>();
        final List<NodeChecker> sparkNodeCheckers = new ArrayList<>();
        final List<UnitChecker> unitCheckers = new ArrayList<>();
        final Map<String, BaseChecker> allCheckers = this.global.getCheckers();

        // Lambda to dispatch checkers in the correct lists
        final BiConsumer<BaseChecker, List<NodeChecker>> dispatchChecker =
                (checker, nodeCheckers) -> {
                    if (checker instanceof NodeChecker nodeChecker) {
                        nodeCheckers.add(nodeChecker);
                        if (nodeChecker.isFollowGenericInstantiations()) {
                            needsToFollowInstantiations = true;
                        }
                    } else {
                        unitCheckers.add((UnitChecker) checker);
                    }
                };

        // If there are no required instance, check if we have to fall back on all checkers
        if (this.getRuleInstances().isEmpty()
                && this.env.getOptions().get(LKQLLanguage.fallbackToAllRules)) {
            for (BaseChecker checker : allCheckers.values()) {
                dispatchChecker.accept(checker, generalNodeCheckers);
            }
        }

        // Iterate over all rule instance to create the checker lists
        else {
            for (Map.Entry<String, RuleInstance> instanceEntry :
                    this.getRuleInstances().entrySet()) {
                final RuleInstance instance = instanceEntry.getValue();

                // Get the checker associated to the rule instance
                BaseChecker checker = allCheckers.get(instance.ruleName().toLowerCase());

                // Verify that the instantiated rule exists
                if (checker == null) {
                    throw LKQLRuntimeException.fromMessage(
                            "Could not find any rule named " + instance.ruleName());
                }

                // If the instance has a custom name, close the checker and set the alias name
                if (instance.instanceName().isPresent()) {
                    checker = checker.copy();
                    checker.setAlias(instance.instanceName().get());
                }

                switch (instance.sourceMode()) {
                    case GENERAL -> dispatchChecker.accept(checker, generalNodeCheckers);
                    case ADA -> dispatchChecker.accept(checker, adaNodeCheckers);
                    case SPARK -> dispatchChecker.accept(checker, sparkNodeCheckers);
                }
            }
        }

        // Set the checker caches
        this.filteredGeneralNodeCheckers = generalNodeCheckers.toArray(new NodeChecker[0]);
        this.filteredAdaNodeCheckers = adaNodeCheckers.toArray(new NodeChecker[0]);
        this.filteredSparkNodeCheckers = sparkNodeCheckers.toArray(new NodeChecker[0]);
        this.filteredUnitCheckers = unitCheckers.toArray(new UnitChecker[0]);
    }

    /**
     * @return whether there is at least one rule that needs to follow generic instantiations.
     */
    public boolean mustFollowInstantiations() {
        return needsToFollowInstantiations;
    }
}
