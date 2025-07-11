//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.checker.BaseChecker;
import com.adacore.lkql_jit.checker.NodeChecker;
import com.adacore.lkql_jit.checker.UnitChecker;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.options.RuleInstance;
import com.adacore.lkql_jit.runtime.CallStack;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.LangkitLocationWrapper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.util.*;
import java.util.function.BiConsumer;
import org.json.JSONObject;
import org.json.JSONTokener;

/**
 * This class represents the execution context of an LKQL script.
 *
 * @author Hugo GUERRIER
 */
public final class LKQLContext {

    private final LKQLLanguage language;

    // ----- Attributes -----

    /** Environment of the language. */
    @CompilerDirectives.CompilationFinal
    private TruffleLanguage.Env env;

    /** The global values of the LKQL execution. */
    private final GlobalScope global;

    public final CheckerUtils.SourceLinesCache linesCache = new CheckerUtils.SourceLinesCache();

    /** The call stack of the current language thread. */
    public final CallStack callStack = new CallStack();

    /** The stack representing the current LKQL source chain. */
    public final Stack<Source> fromStack = new Stack<>();

    // ----- Ada project attributes -----

    /** The analysis context for the ada files. */
    private LangkitSupport.AnalysisContextInterface analysisContext;

    /** The rewriting context, opened from the Ada analysis context. */
    private LangkitSupport.RewritingContextInterface rewritingContext;

    /** The project manager for the ada project. */
    private Libadalang.ProjectManager projectManager;

    /** Event handler for the project manager. */
    private final Libadalang.EventHandler eventHandler = Libadalang.EventHandler.create(
        (ctx, name, from, found, notFoundIsError) -> {
            if (!found && notFoundIsError) {
                boolean isFatal = !this.keepGoingOnMissingFile();
                this.getDiagnosticEmitter()
                    .emitFileNotFound(
                        new LangkitLocationWrapper(from.getRoot(), this.linesCache),
                        name,
                        isFatal
                    );
                if (isFatal) {
                    this.env.getContext().closeExited(null, 1);
                }
            }
        },
        null
    );

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
    private LangkitSupport.AnalysisUnit[] specifiedUnits;

    /** All the units of the project, including those of its non-externally-built dependencies. */
    private LangkitSupport.AnalysisUnit[] allUnits;

    /** The root nodes of all the analysis units of the project. */
    private LangkitSupport.NodeInterface[] allUnitsRoots;

    // ----- Checker attributes -----

    /** A cache for all rule arguments to avoid evaluating twice the same argument source. */
    private Map<String, Map<String, Object>> instancesArgsCache = new HashMap<>();

    /** Whether there is at least one rule that needs to follow generic instantiations. */
    private boolean needsToFollowInstantiations = false;

    /** Node checkers to run on all nodes from the Ada sources. */
    private NodeChecker[] filteredGeneralNodeCheckers = null;

    /** Node checkers to run on non-SPARK nodes from the Ada sources. */
    private NodeChecker[] filteredNodeCheckers = null;

    /** Node checkers to run only on SPARK nodes from the Ada sources. */
    private NodeChecker[] filteredSparkNodeCheckers = null;

    /** Unit checkers to run. */
    private UnitChecker[] filteredUnitCheckers = null;

    // ----- Option caches -----

    /** Options object passed to the LKQL engine. */
    @CompilerDirectives.CompilationFinal
    private LKQLOptions options = null;

    /** The project's scenario variables. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private Libadalang.ScenarioVariable[] scenarioVars = null;

    /** Directories to look for LKQL rules into. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] ruleDirectories = null;

    /** Tool to emit diagnostics in the wanted format. */
    @CompilerDirectives.CompilationFinal
    private CheckerUtils.DiagnosticEmitter emitter;

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
        if (this.rewritingContext != null && !this.rewritingContext.isClosed()) {
            this.rewritingContext.close();
        }
        this.eventHandler.close();
        this.analysisContext.close();
        if (this.projectManager != null) this.projectManager.close();
    }

    // ----- Getters -----

    public TruffleLanguage.Env getEnv() {
        return this.env;
    }

    public GlobalScope getGlobal() {
        return this.global;
    }

    public LangkitSupport.AnalysisUnit[] getSpecifiedUnits() {
        if (!this.parsed) {
            this.parseSources();
        }
        return this.specifiedUnits;
    }

    public LangkitSupport.AnalysisUnit[] getAllUnits() {
        if (!this.parsed) {
            this.parseSources();
        }
        return this.allUnits;
    }

    public LangkitSupport.NodeInterface[] getAllUnitsRoots() {
        if (!this.parsed) {
            this.parseSources();
        }
        return this.allUnitsRoots;
    }

    public boolean hasRewritingContext() {
        return this.rewritingContext != null;
    }

    public LangkitSupport.RewritingContextInterface getRewritingContext() {
        if (this.rewritingContext == null) {
            if (!this.parsed) {
                this.parseSources();
            }
            this.rewritingContext = this.analysisContext.startRewriting();
        }
        return this.rewritingContext;
    }

    /**
     * Try applying the current rewriting context, reparsing the rewrote analysis unit. If this
     * operation is a success then discard the current rewriting context. Otherwise, close it. This
     * method assumes that the current rewriting context is not null.
     */
    public LangkitSupport.RewritingApplyResult applyOrCloseRewritingContext() {
        final var res = this.rewritingContext.apply();
        if (!res.success) {
            this.rewritingContext.close();
        }
        this.rewritingContext = null;
        return res;
    }

    // ----- Setters -----

    public void patchContext(TruffleLanguage.Env newEnv) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        this.env = newEnv;
        this.invalidateOptionCaches();
        this.initSources();
    }

    // ----- Options getting methods -----

    /** Parse the LKQL engine options passed as a JSON string, store it in a cache and return it. */
    @CompilerDirectives.TruffleBoundary
    public LKQLOptions getOptions() {
        if (this.options == null) {
            final var optionsSource = this.env.getOptions().get(LKQLLanguage.options);

            // If the "lkql.options" value is empty, get default options.
            if (!optionsSource.isBlank()) {
                final var jsonObject = new JSONObject(new JSONTokener(optionsSource));
                this.options = LKQLOptions.fromJson(jsonObject);
            } else {
                this.options = LKQLOptions.getDefault();
            }
        }
        return this.options;
    }

    public LKQLOptions.EngineMode getEngineMode() {
        return this.getOptions().engineMode();
    }

    /**
     * Get if the language execution is in verbose mode.
     *
     * @return True if the verbose flag is on.
     */
    public boolean isVerbose() {
        return this.getOptions().verbose();
    }

    /** Return true if the engine should keep running when a required file is not found. */
    public boolean keepGoingOnMissingFile() {
        return this.getOptions().keepGoingOnMissingFile();
    }

    /** Get whether to display instantiation chain in diagnostics. */
    public boolean showInstantiationChain() {
        return this.getOptions().showInstantiationChain();
    }

    /**
     * Return the project file of the language context.
     *
     * @return The project file in a string.
     */
    public String getProjectFile() {
        return this.getOptions().projectFile().orElse("");
    }

    public String getTarget() {
        return this.getOptions().target().orElse("");
    }

    public String getRuntime() {
        return this.getOptions().runtime().orElse("");
    }

    public String getConfigFile() {
        return this.getOptions().configFile().orElse("");
    }

    @CompilerDirectives.TruffleBoundary
    public TruffleLogger getLogger() {
        return TruffleLogger.getLogger(Constants.LKQL_ID);
    }

    /** Return the list of scenario variables to specify when loading the GPR project file. */
    public Libadalang.ScenarioVariable[] getScenarioVars() {
        if (this.scenarioVars == null) {
            final var scenarioVarList = new ArrayList<Libadalang.ScenarioVariable>();
            for (var entry : this.getOptions().scenarioVariables().entrySet()) {
                scenarioVarList.add(
                    Libadalang.ScenarioVariable.create(entry.getKey(), entry.getValue())
                );
            }
            this.scenarioVars = scenarioVarList.toArray(new Libadalang.ScenarioVariable[0]);
        }
        return this.scenarioVars;
    }

    /**
     * Get the files to analyse.
     *
     * @return The files to analyse in an array.
     */
    public String[] getFiles() {
        return this.getOptions().files().toArray(new String[0]);
    }

    /**
     * Get whether the checker is in debug mode.
     *
     * @return True if the checker is in debug mode, false else
     */
    @CompilerDirectives.TruffleBoundary
    public boolean isCheckerDebug() {
        return this.getOptions().checkerDebug();
    }

    @CompilerDirectives.TruffleBoundary
    public LKQLOptions.AutoFixMode getAutoFixMode() {
        return this.getOptions().autoFixMode();
    }

    /**
     * Get the directories to get the rules from.
     *
     * @return The directory array.
     */
    public String[] getRuleDirectories() {
        if (this.ruleDirectories == null) {
            final var rulesDirsList = new ArrayList<>(this.getOptions().rulesDirs());
            final var additionalRulesDirs = System.getenv(Constants.LKQL_RULES_PATH);
            if (additionalRulesDirs != null) {
                rulesDirsList.addAll(Arrays.asList(StringUtils.splitPaths(additionalRulesDirs)));
            }
            rulesDirsList.sort(null);
            this.ruleDirectories = rulesDirsList.toArray(new String[0]);
        }
        return this.ruleDirectories;
    }

    /**
     * Get the Ada file to ignore during the analysis.
     *
     * @return The array containing all Ada files to ignore.
     */
    public String[] getIgnores() {
        return this.getOptions().ignores().toArray(new String[0]);
    }

    /** Invalidate the option caches. */
    private void invalidateOptionCaches() {
        this.options = null;
        this.instancesArgsCache = new HashMap<>();
        this.emitter = null;
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
            this.emitter = switch (this.getOptions().diagnosticOutputMode()) {
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
                .filter(source -> {
                    for (String ignore : ignores) {
                        if (source.contains(ignore)) return false;
                    }
                    return true;
                })
                .toArray(String[]::new);

        // For each specified source file, store its corresponding analysis unit in the list of
        // specified units
        this.specifiedUnits = new LangkitSupport.AnalysisUnit[usedSources.length];
        for (int i = 0; i < usedSources.length; i++) {
            this.specifiedUnits[i] = this.analysisContext.getUnitFromFile(usedSources[i]);
        }

        // For each source file of the project, store its corresponding analysis unit in the list of
        // all
        // the units
        // of the project, as well as their root nodes.
        this.allUnits = new LangkitSupport.AnalysisUnit[this.allSourceFiles.size()];
        this.allUnitsRoots = new LangkitSupport.NodeInterface[this.allSourceFiles.size()];

        for (int i = 0; i < this.allUnits.length; i++) {
            this.allUnits[i] = this.analysisContext.getUnitFromFile(this.allSourceFiles.get(i));
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
            this.projectManager = Libadalang.ProjectManager.create(
                projectFileName,
                this.getScenarioVars(),
                this.getTarget(),
                this.getRuntime(),
                this.getConfigFile(),
                true
            );

            // Forward the project diagnostics if there are some
            if (!this.projectManager.getDiagnostics().isEmpty()) {
                this.getDiagnosticEmitter()
                    .emitProjectErrors(
                        new File(projectFileName).getName(),
                        this.projectManager.getDiagnostics()
                    );
            }

            // Get the subproject provided by the user
            final String[] subprojects =
                this.getOptions().subprojectFile().map(s -> new String[] { s }).orElse(null);

            // If no files were specified by the user, the files to analyze are those of the root
            // project (i.e. without recursing into project dependencies)
            if (this.specifiedSourceFiles.isEmpty()) {
                this.specifiedSourceFiles.addAll(
                        Arrays.stream(
                            this.projectManager.getFiles(
                                    Libadalang.SourceFileMode.ROOT_PROJECT,
                                    subprojects
                                )
                        ).toList()
                    );
            }

            // The `units()` built-in function must return all units of the project including units
            // from its dependencies. So let's retrieve all those files as well.
            this.allSourceFiles.addAll(
                    Arrays.stream(
                        this.projectManager.getFiles(
                                Libadalang.SourceFileMode.WHOLE_PROJECT,
                                subprojects
                            )
                    ).toList()
                );

            this.analysisContext = this.projectManager.createContext(
                    this.getOptions().subprojectFile().orElse(null),
                    this.eventHandler,
                    true,
                    8
                );
        }
        // Else, either load the implicit project.
        else {
            // We should not get any scenario variable if we are being run without a project file.
            if (this.getScenarioVars().length != 0) {
                throw LKQLRuntimeException.fromMessage(
                    "Scenario variable specifications require a project file"
                );
            }

            // If the option is the empty string, the language implementation will end up setting it
            // to the default value for its language (e.g. iso-8859-1 for Ada).
            String charset = this.getOptions().charset().orElse("");

            // Load the implicit project
            this.projectManager = Libadalang.ProjectManager.createImplicit(
                this.getTarget(),
                this.getRuntime(),
                this.getConfigFile(),
                true
            );
            this.allSourceFiles.addAll(
                    Arrays.stream(
                        this.projectManager.getFiles(Libadalang.SourceFileMode.WHOLE_PROJECT)
                    ).toList()
                );
            final Libadalang.UnitProvider provider = this.projectManager.getProvider();

            // Create the ada context and store it in the LKQL context
            /*
             * TODO: Genericize LKQL or Java issue #502. Requires to make create static but not possible
             * via an interface nor an abstract class.
             */
            this.analysisContext = Libadalang.AnalysisContext.create(
                charset,
                null,
                provider,
                this.eventHandler,
                true,
                8
            );

            // In the absence of a project file, we consider for now that there are no configuration
            // pragmas.
            this.analysisContext.setConfigPragmasMapping(null, null);
        }
    }

    // ----- Checker methods -----

    @CompilerDirectives.TruffleBoundary
    public Map<String, RuleInstance> getRuleInstances() {
        return this.getOptions().ruleInstances();
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
                    var unit = context.getUnitFromBuffer(
                        argSource,
                        "<rule_arg>",
                        null,
                        Liblkqllang.GrammarRule.EXPR_RULE
                    );
                    var root = unit.getRoot();
                    var source = Source.newBuilder(
                        Constants.LKQL_ID,
                        argSource,
                        "<rule_arg>"
                    ).build();
                    var node = language.translate(root, source);
                    argValue = node.executeGeneric(null);
                } catch (Exception e) {
                    throw LKQLRuntimeException.fromMessage(
                        "Rule argument value generated an " +
                        "interpreter error: '" +
                        argSource +
                        "'"
                    );
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
    public NodeChecker[] getNodeCheckers() {
        if (this.filteredNodeCheckers == null) {
            this.initCheckerCaches();
        }
        return this.filteredNodeCheckers;
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
        final List<NodeChecker> nodeCheckers = new ArrayList<>();
        final List<NodeChecker> sparkNodeCheckers = new ArrayList<>();
        final List<UnitChecker> unitCheckers = new ArrayList<>();
        final Map<String, BaseChecker> allCheckers = this.global.getCheckers();

        // Lambda to dispatch checkers in the correct lists
        final BiConsumer<BaseChecker, List<NodeChecker>> dispatchChecker = (
            checker,
            nodeCheckerList
        ) -> {
            if (checker instanceof NodeChecker nodeChecker) {
                nodeCheckerList.add(nodeChecker);
                if (nodeChecker.isFollowGenericInstantiations()) {
                    needsToFollowInstantiations = true;
                }
            } else {
                unitCheckers.add((UnitChecker) checker);
            }
        };

        // If there are no required instance, check if we have to fall back on all checkers
        if (this.getRuleInstances().isEmpty() && this.getOptions().fallbackToAllRules()) {
            for (BaseChecker checker : allCheckers.values()) {
                dispatchChecker.accept(checker, generalNodeCheckers);
            }
        }
        // Iterate over all rule instance to create the checker lists
        else {
            for (Map.Entry<String, RuleInstance> instanceEntry : this.getRuleInstances()
                .entrySet()) {
                final RuleInstance instance = instanceEntry.getValue();

                // Get the checker associated to the rule instance
                BaseChecker checker = allCheckers.get(instance.ruleName().toLowerCase());

                // Verify that the instantiated rule exists
                if (checker == null) {
                    throw LKQLRuntimeException.fromMessage(
                        "Could not find any rule named " + instance.ruleName()
                    );
                }

                // If the engine is in "fixer" mode, check that the rule has an auto-fixing function
                if (getEngineMode() == LKQLOptions.EngineMode.FIXER && checker.autoFix == null) {
                    throw LKQLRuntimeException.fromMessage(
                        "Rule \"" +
                        instance.ruleName() +
                        "\" is not defining any auto-fixing function"
                    );
                }

                // If the instance has a custom name, close the checker and set the alias name
                if (instance.instanceName().isPresent()) {
                    checker = checker.copy();
                    checker.setAlias(instance.instanceName().get());
                }

                switch (instance.sourceMode()) {
                    case GENERAL -> dispatchChecker.accept(checker, generalNodeCheckers);
                    case ADA -> dispatchChecker.accept(checker, nodeCheckers);
                    case SPARK -> dispatchChecker.accept(checker, sparkNodeCheckers);
                }
            }
        }

        // Set the checker caches
        this.filteredGeneralNodeCheckers = generalNodeCheckers.toArray(new NodeChecker[0]);
        this.filteredNodeCheckers = nodeCheckers.toArray(new NodeChecker[0]);
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
