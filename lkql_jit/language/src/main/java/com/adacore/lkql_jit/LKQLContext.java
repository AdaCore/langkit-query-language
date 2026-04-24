//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.checker.BaseChecker;
import com.adacore.lkql_jit.checker.NodeChecker;
import com.adacore.lkql_jit.checker.UnitChecker;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.exceptions.LogLocation;
import com.adacore.lkql_jit.langkit_translator.passes.Hierarchy;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.options.RuleInstance;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.LangkitLocationWrapper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.logging.Level;
import java.util.stream.Stream;
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

    /** The stack representing the current LKQL source chain. */
    public final Stack<Source> fromStack = new Stack<>();

    // ----- Ada project attributes -----

    /** The analysis context for the ada files. */
    private LangkitSupport.AnalysisContextInterface analysisContext;

    /** The project manager for the ada project. */
    private Libadalang.ProjectManager projectManager;

    /** Event handler for the project manager. */
    private final Libadalang.EventHandler eventHandler = Libadalang.EventHandler.create(
        (ctx, name, from, found, notFoundIsError) -> {
            if (!found && notFoundIsError) {
                // Get the base name of the requested Ada file and extract the unit name from it
                var adaFileName = Paths.get(name).getFileName().toString();
                var split = adaFileName.split("\\.");
                var requestedUnitName = Libadalang.Symbol.create(split[0]);

                // Try to get the "with" statement that caused this event
                var reportLocationNode = from
                    .getRoot()
                    .walk()
                    .filter(
                        n ->
                            n instanceof Libadalang.WithClause wc &&
                            wc
                                .fPackages()
                                .walk()
                                .anyMatch(
                                    p ->
                                        p instanceof Libadalang.Name pn &&
                                        pn.pNameIs(requestedUnitName)
                                )
                    )
                    .findFirst()
                    .map(n -> (Libadalang.AdaNode) n)
                    .orElse(from.getRoot());

                // Now report the error
                var level = missingFileIsError() ? Level.SEVERE : Level.WARNING;
                var message = "File " + adaFileName + " not found";
                if (this.getEngineMode() == LKQLOptions.EngineMode.CHECKER) {
                    this.getDiagnosticEmitter().emitFileNotFound(
                        new LangkitLocationWrapper(reportLocationNode, this.linesCache),
                        name,
                        missingFileIsError()
                    );
                } else {
                    this.getLogger().log(
                        level,
                        message,
                        new LogLocation(
                            new LogLocation.LangkitLocation(
                                from,
                                reportLocationNode.getSourceLocationRange()
                            )
                        )
                    );
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

    /** Directories to look for LKQL rules into. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] ruleDirectories = null;

    /** Tool to emit diagnostics in the wanted format. */
    @CompilerDirectives.CompilationFinal
    private CheckerUtils.DiagnosticEmitter emitter;

    // ----- Nanopass typing context -----

    /** Typing context used by pattern-matching during a rewriting pass */
    private Hierarchy typingContext = null;

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
        this.language = language;
    }

    // ----- Destructors -----

    /** Finalize the LKQL context to close libadalang context. */
    public void finalizeContext() {
        eventHandler.close();
        if (analysisContext != null) analysisContext.close();
        if (projectManager != null) projectManager.close();
    }

    // ----- Getters -----

    public TruffleLanguage.Env getEnv() {
        return this.env;
    }

    public GlobalScope getGlobal() {
        return this.global;
    }

    public Stream<LangkitSupport.AnalysisUnit> getSpecifiedUnits() {
        String[] ignores = this.getIgnores();
        return this.specifiedSourceFiles.stream()
            .filter(source -> Arrays.stream(ignores).noneMatch(source::contains))
            .map(f -> analysisContext.getUnitFromFile(f));
    }

    public Stream<LangkitSupport.AnalysisUnit> getAllUnits() {
        String[] ignores = this.getIgnores();
        return this.allSourceFiles.stream().map(f -> {
            return analysisContext.getUnitFromFile(f);
        });
    }

    @CompilerDirectives.TruffleBoundary
    public LangkitSupport.NodeInterface[] allUnitsRoots() {
        return getAllUnits()
            .map(u -> u.getRoot())
            .toArray(LangkitSupport.NodeInterface[]::new);
    }

    public LangkitSupport.AnalysisContextInterface getAnalysisContext() {
        return this.analysisContext;
    }

    public LangkitSupport.RewritingContextInterface getRewritingContext() {
        var ctx = this.analysisContext.getRewritingContext();
        if (ctx == null) {
            return this.analysisContext.startRewriting();
        }
        return ctx;
    }

    public Hierarchy getTypingContext() {
        return typingContext;
    }

    // ----- Setters -----

    public void patchContext(TruffleLanguage.Env newEnv) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        this.env = newEnv;
        this.invalidateOptionCaches();
        this.initSources();
    }

    public void setTypingContext(Hierarchy typingContext) {
        this.typingContext = typingContext;
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
    public boolean missingFileIsError() {
        return this.getOptions().missingFileIsError();
    }

    /** Get whether to display instantiation chain in diagnostics. */
    public boolean showInstantiationChain() {
        return this.getOptions().showInstantiationChain();
    }

    @CompilerDirectives.TruffleBoundary
    public TruffleLogger getLogger() {
        return TruffleLogger.getLogger(Constants.LKQL_ID);
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

    /**
     * Get the directories to get the rules from.
     *
     * @return The directory array.
     */
    public String[] getRuleDirectories() {
        if (this.ruleDirectories == null) {
            final var rulesDirsList = new ArrayList<>(this.getOptions().rulesDirs());
            final var additionalRulesDirs = System.getenv(Constants.LKQL_PATH);
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

    /** Initialize the ada sources. */
    public void initSources() {
        // Clear the context caches
        this.specifiedSourceFiles.clear();
        this.allSourceFiles.clear();
        var options = getOptions();

        // Store all the user-specified files to process after verifying they exist
        for (String file : this.getFiles()) {
            if (!file.isBlank()) {
                File sourceFile = new File(file);
                if (sourceFile.isFile()) {
                    this.specifiedSourceFiles.add(sourceFile.getAbsolutePath());
                } else {
                    var level = missingFileIsError() ? Level.SEVERE : Level.WARNING;
                    var message = "File " + sourceFile.getName() + " not found";
                    this.getLogger().log(level, message);
                }
            }
        }

        try (Libadalang.ProjectOptions gprOptions = new Libadalang.ProjectOptions()) {
            var projectFile = options.projectFile();

            // We should not get any scenario variable if we are being run without a project file.
            if (projectFile.isEmpty() && !options.scenarioVariables().isEmpty()) {
                throw LKQLEngineException.create(
                    "Scenario variable specifications require a project file"
                );
            }

            // Create the GPR options object
            options
                .additionalProjectPaths()
                .forEach(p -> gprOptions.addSwitch(Libadalang.ProjectOption.AP, p));
            options
                .autoconf()
                .ifPresent(a -> gprOptions.addSwitch(Libadalang.ProjectOption.AUTOCONF, a));
            options
                .configFile()
                .ifPresent(c -> gprOptions.addSwitch(Libadalang.ProjectOption.CONFIG, c));
            options
                .additionalKnowledgeBases()
                .forEach(kb -> gprOptions.addSwitch(Libadalang.ProjectOption.DB, kb));
            if (options.skipStandardKnowledgeBase()) gprOptions.addSwitch(
                Libadalang.ProjectOption.DB_MINUS
            );
            options
                .implicitWiths()
                .forEach(iw -> gprOptions.addSwitch(Libadalang.ProjectOption.IMPLICIT_WITH, iw));
            if (options.followSymlinks()) gprOptions.addSwitch(
                Libadalang.ProjectOption.RESOLVE_LINKS
            );
            if (options.noProject()) gprOptions.addSwitch(Libadalang.ProjectOption.NO_PROJECT);
            projectFile.ifPresent(p -> gprOptions.addSwitch(Libadalang.ProjectOption.P, p));
            options
                .rootDir()
                .ifPresent(d -> gprOptions.addSwitch(Libadalang.ProjectOption.ROOT_DIR, d));
            options
                .relocateBuildTree()
                .ifPresent(d ->
                    gprOptions.addSwitch(Libadalang.ProjectOption.RELOCATE_BUILD_TREE, d)
                );
            options
                .adaRuntime()
                .ifPresent(r -> gprOptions.addSwitch(Libadalang.ProjectOption.RTS, r));
            options
                .runtimes()
                .forEach((l, r) -> gprOptions.addSwitch(Libadalang.ProjectOption.RTS, r, l));
            options
                .srcSubdirs()
                .ifPresent(d -> gprOptions.addSwitch(Libadalang.ProjectOption.SRC_SUBDIRS, d));
            options
                .subdirs()
                .ifPresent(d -> gprOptions.addSwitch(Libadalang.ProjectOption.SUBDIRS, d));
            options
                .target()
                .ifPresent(t -> gprOptions.addSwitch(Libadalang.ProjectOption.TARGET, t));
            options
                .scenarioVariables()
                .forEach((n, v) -> gprOptions.addSwitch(Libadalang.ProjectOption.X, n + "=" + v));

            // Create the Libadalang project manager
            projectManager = new Libadalang.ProjectManager(gprOptions, true);

            // Filter project diagnostics and fetch the implicitly loaded project file if any
            var diagnosticsToForward = new ArrayList<String>();
            var loadedProject = projectFile;
            for (var diag : projectManager.getDiagnostics()) {
                if (diag.startsWith("using project file")) {
                    var split = diag.split(" ");
                    loadedProject = Optional.of(split[split.length - 1]);
                } else if (!diag.startsWith("use implicit project")) {
                    diagnosticsToForward.add(diag);
                }
            }

            // Forward project diagnostics only if a project file has been loaded
            if (!diagnosticsToForward.isEmpty() && loadedProject.isPresent()) {
                for (var diagnostic : diagnosticsToForward) {
                    getLogger().severe(diagnostic);
                }
            }

            // Create an array containing the requested subproject if any
            var subprojects = options
                .subprojectFile()
                .map(f -> new String[] { f })
                .orElse(null);

            // If no files were specified by the user, the files to analyze are those of the root
            // project (i.e. without recursing into project dependencies)
            if (specifiedSourceFiles.isEmpty()) {
                specifiedSourceFiles.addAll(
                    List.of(
                        projectManager.getFiles(Libadalang.SourceFileMode.ROOT_PROJECT, subprojects)
                    )
                );
            }

            // The 'units()' built-in function must return all units of the project including units
            // from its dependencies. So let's retrieve all those files as well.
            allSourceFiles.addAll(
                List.of(
                    projectManager.getFiles(Libadalang.SourceFileMode.WHOLE_PROJECT, subprojects)
                )
            );

            // Finally create an analysis context from the loaded project
            analysisContext = projectManager.createContext(
                options.subprojectFile().orElse(null),
                options.charset().orElse(null),
                eventHandler,
                true,
                8
            );
        } catch (Libadalang.ProjectManagerException e) {
            throw LKQLEngineException.create(e);
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
        final Map<String, Object> instanceArgs = this.instancesArgsCache.getOrDefault(
            instanceId,
            new HashMap<>()
        );

        // If an argument is not already in the argument values cache, get its source from the
        // registered instances and evaluate it if some.
        if (!instanceArgs.containsKey(argName)) {
            final RuleInstance instance = this.getRuleInstances().get(instanceId);
            final String argSource = instance == null ? null : instance.arguments().get(argName);
            final Object argValue;
            if (argSource == null) {
                argValue = null;
            } else {
                var tl = ((TopLevelList) language.translate(argSource, "<rule-arg>")).program;
                var node = (Expr) tl[0];
                argValue = node.executeGeneric(null);
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
            for (Map.Entry<
                String,
                RuleInstance
            > instanceEntry : this.getRuleInstances().entrySet()) {
                final RuleInstance instance = instanceEntry.getValue();

                // Get the checker associated to the rule instance
                BaseChecker checker = allCheckers.get(instance.ruleName().toLowerCase());

                // Verify that the instantiated rule exists
                if (checker == null) {
                    throw LKQLEngineException.create(
                        "Could not find any rule named " + instance.ruleName()
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
