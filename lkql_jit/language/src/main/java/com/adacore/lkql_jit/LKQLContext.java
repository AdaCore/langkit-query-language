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

package com.adacore.lkql_jit;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLConfigFileResult;
import com.adacore.lkql_jit.utils.checkers.BaseChecker;
import com.adacore.lkql_jit.utils.checkers.NodeChecker;
import com.adacore.lkql_jit.utils.checkers.UnitChecker;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.functions.CheckerUtils;
import com.adacore.lkql_jit.utils.functions.ParsingUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.*;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * This class represents the execution context of an LKQL script.
 *
 * @author Hugo GUERRIER
 */
public final class LKQLContext {

    // ----- Attributes -----

    /** Environment of the language. */
    @CompilerDirectives.CompilationFinal private TruffleLanguage.Env env;

    /** The global values of the LKQL execution. */
    private final GlobalScope global;

    // ----- Ada project attributes -----

    /** The analysis context for the ada files. */
    private Libadalang.AnalysisContext adaContext;

    /** The project manager for the ada project. */
    private Libadalang.ProjectManager projectManager;

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

    /** Result of the LKQL config file parsing and execution. */
    private LKQLConfigFileResult ruleConfigFileResult = null;

    /** List containing all rules to run on all code (SPARK and Ada). */
    private List<String> allRules = null;

    /** List containing rules to apply only to Ada code. */
    private List<String> adaRules = null;

    /** List containing rules to apply only to SPARK code. */
    private List<String> sparkRules = null;

    /** Aliases of the rules. */
    private Map<String, String> allAliases = null;

    /** The rules arguments. */
    private Map<String, Map<String, Object>> allRulesArgs = null;

    /** The filtered node checkers cache. */
    private NodeChecker[] filteredAllNodeCheckers = null;

    /** The filtered node checkers for Ada code only. */
    private NodeChecker[] filteredAdaNodeCheckers = null;

    /** The filtered node checkers for SPARK code only. */
    private NodeChecker[] filteredSparkNodeCheckers = null;

    /** The filtered unit checkers cache. */
    private UnitChecker[] filteredUnitCheckers = null;

    /** Whether there is at least one rule that needs to follow generic instantiations. */
    private boolean needsToFollowInstantiations = false;

    // ----- Option caches -----

    /** Whether the language is in the verbose mode. */
    @CompilerDirectives.CompilationFinal private Boolean isVerbose = null;

    @CompilerDirectives.CompilationFinal private Boolean keepGoingOnMissingFile = null;

    /** The project file to analyse. */
    @CompilerDirectives.CompilationFinal private String projectFile = null;

    /** The project's scenario variables. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private Libadalang.ScenarioVariable[] scenarioVars = null;

    /** The ada files passed through the command line. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] files = null;

    /** The error printing mode. */
    @CompilerDirectives.CompilationFinal private String errorMode = null;

    /** Whether the checker is in debug mode. */
    @CompilerDirectives.CompilationFinal private Boolean checkerDebug = null;

    /** The rules to execute. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] specifiedRules;

    /** The LKQL file containing rules configuration. */
    @CompilerDirectives.CompilationFinal private String ruleConfigFile;

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
    public LKQLContext(TruffleLanguage.Env env, GlobalScope global) {
        this.env = env;
        this.global = global;
        this.specifiedSourceFiles = new ArrayList<>();
        this.allSourceFiles = new ArrayList<>();
        this.parsed = false;
    }

    // ----- Destructors -----

    /** Finalize the LKQL context to close libadalang context. */
    public void finalizeContext() {
        this.adaContext.close();
        if (this.projectManager != null) this.projectManager.close();
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
        return this.env.getOptions().get(LKQLLanguage.target);
    }

    public String getRuntime() {
        return this.env.getOptions().get(LKQLLanguage.runtime);
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
     * Get the error handling mode.
     *
     * @return The mode in a string.
     */
    public String getErrorMode() {
        if (this.errorMode == null) {
            this.errorMode = this.env.getOptions().get(LKQLLanguage.errorMode);
        }
        return this.errorMode;
    }

    /**
     * Get the rule to run with the checker.
     *
     * @return The rule to run.
     */
    @CompilerDirectives.TruffleBoundary
    private String[] getSpecifiedRules() {
        if (this.specifiedRules == null) {
            String[] unfilteredRules =
                    this.env
                            .getOptions()
                            .get(LKQLLanguage.rules)
                            .trim()
                            .replace(" ", "")
                            .split(",");
            this.specifiedRules =
                    Arrays.stream(unfilteredRules)
                            .filter(s -> !s.isBlank() && !s.isEmpty())
                            .map(String::toLowerCase)
                            .distinct()
                            .toArray(String[]::new);
        }
        return this.specifiedRules;
    }

    /**
     * Get the LKQL file to configure the rules.
     *
     * @return The LKQL file name to configure the rules.
     */
    private String getRuleConfigFile() {
        if (this.ruleConfigFile == null) {
            this.ruleConfigFile = this.env.getOptions().get(LKQLLanguage.LKQLRuleFile);
        }
        return this.ruleConfigFile;
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
        this.errorMode = null;
        this.specifiedRules = null;
        this.ruleConfigFile = null;
        this.ruleDirectories = null;
        this.ignores = null;
        this.emitter = null;

        this.ruleConfigFileResult = null;
        this.allRules = null;
        this.adaRules = null;
        this.sparkRules = null;
        this.allAliases = null;
        this.allRulesArgs = null;
    }

    // ----- Value related methods -----

    /**
     * Get the meta table for the given type.
     *
     * @param type The type to get the meta table for.
     * @return The meta table for the type.
     */
    public Map<String, BuiltInFunctionValue> getMetaTable(String type) {
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
        // Prepare the list of ada files to analyse
        this.specifiedSourceFiles.clear();
        this.allSourceFiles.clear();

        // Add all the user-specified files to process after verifying they exist
        for (String file : this.getFiles()) {
            if (!file.isEmpty() && !file.isBlank()) {
                File sourceFile = new File(file);
                if (sourceFile.isFile()) {
                    this.specifiedSourceFiles.add(sourceFile.getAbsolutePath());
                } else {
                    this.getDiagnosticEmitter()
                            .emitMissingFile(null, file, !this.keepGoingOnMissingFile(), this);
                }
            }
        }

        // Setup the event handler
        final Libadalang.EventHandler.UnitRequestedCallback unitRequested =
                (ctx, name, from, found, not_found_is_error) -> {
                    if (!found && not_found_is_error) {
                        boolean isFatal = !this.keepGoingOnMissingFile();
                        this.getDiagnosticEmitter().emitMissingFile(from, name, isFatal, this);
                        if (isFatal) {
                            this.env.getContext().closeExited(null, 1);
                        }
                    }
                };
        final Libadalang.EventHandler eventHandler =
                Libadalang.EventHandler.create(unitRequested, null);

        // If the option is the empty string, the language implementation will end up setting it to
        // the
        // default
        // value for its language (e.g. iso-8859-1 for Ada).
        String charset = this.env.getOptions().get(LKQLLanguage.charset);

        // Get the project file and parse it if there is one
        String projectFileName = this.getProjectFile();

        if (projectFileName != null && !projectFileName.isEmpty() && !projectFileName.isBlank()) {
            // Create the project manager
            this.projectManager =
                    Libadalang.ProjectManager.create(
                            projectFileName,
                            this.getScenarioVars(),
                            this.getTarget(),
                            this.getRuntime());

            // Test if there is any diagnostic in the project manager
            if (!this.projectManager.getDiagnostics().isEmpty()) {
                throw LKQLRuntimeException.fromMessage(
                        "Error(s) during project opening: " + this.projectManager.getDiagnostics());
            }

            final String subprojectName = this.env.getOptions().get(LKQLLanguage.subprojectFile);
            final String[] subprojects =
                    subprojectName.isEmpty() ? null : new String[] {subprojectName};

            // If no files were specified by the user, the files to analyze are those of the root
            // project
            // (i.e. without recusing into project dependencies)
            if (this.specifiedSourceFiles.isEmpty()) {
                this.specifiedSourceFiles =
                        Arrays.stream(
                                        this.projectManager.getFiles(
                                                Libadalang.SourceFileMode.ROOT_PROJECT,
                                                subprojects))
                                .toList();
            }

            // The `units()` built-in function must return all units of the project including units
            // from
            // its
            // dependencies. So let's retrieve all those files as well.
            this.allSourceFiles =
                    Arrays.stream(
                                    this.projectManager.getFiles(
                                            Libadalang.SourceFileMode.WHOLE_PROJECT, subprojects))
                            .toList();

            this.adaContext =
                    this.projectManager.createContext(
                            subprojectName.isEmpty() ? null : subprojectName,
                            eventHandler,
                            true,
                            8);
        } else {
            // When no project is specified, `units()` should return the same set of units as
            // `specified_units()`.
            this.allSourceFiles = this.specifiedSourceFiles;

            // If required, create an auto provider with the specified files.
            final Libadalang.UnitProvider provider;
            if (this.env.getOptions().get(LKQLLanguage.useAutoProvider)) {
                final List<String> allFiles = this.fetchAdaRuntimeFiles();
                allFiles.addAll(this.allSourceFiles);
                provider = Libadalang.createAutoProvider(allFiles.toArray(new String[0]), charset);
            } else {
                provider = null;
            }

            // We should not get any scenario variable if we are being run without a project file.
            if (this.getScenarioVars().length != 0) {
                throw LKQLRuntimeException.fromMessage(
                        "Scenario variable specifications require a project file");
            }

            this.adaContext =
                    Libadalang.AnalysisContext.create(
                            charset, null, provider, eventHandler, true, 8);

            // In the absence of a project file, we consider for now that there are no configuration
            // pragmas.
            this.adaContext.setConfigPragmasMapping(null, null);
        }

        eventHandler.close();

        // The retrieved source files are not yet parsed
        this.parsed = false;
    }

    /**
     * Return the list of files that belong to the available runtime. We only return specification
     * files, as implementation are not useful for resolving names and types in the actual user
     * sources. If a GNAT installation is not available in the PATH, this returns an empty list
     * without error.
     */
    @CompilerDirectives.TruffleBoundary
    public List<String> fetchAdaRuntimeFiles() {
        final List<String> runtimeFiles = new ArrayList<>();
        try {
            final Process gnatls = new ProcessBuilder("gnatls", "-v").start();
            final BufferedReader reader =
                    new BufferedReader(new InputStreamReader(gnatls.getInputStream()));
            final Optional<String> adaIncludePath =
                    reader.lines().filter(line -> line.contains("adainclude")).findFirst();
            adaIncludePath.ifPresent(
                    path -> {
                        final Path adaIncludeDir = Paths.get(path.trim());
                        try (DirectoryStream<Path> stream =
                                Files.newDirectoryStream(adaIncludeDir, "*.ads")) {
                            stream.forEach(file -> runtimeFiles.add(file.toString()));
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    });
        } catch (IOException ignored) {
            // No runtime available, not a problem
        }
        return runtimeFiles;
    }

    // ----- Checker methods -----

    /**
     * Get the LQKL rule configuration file parsing result.
     *
     * @return The LKQL rule configuration file parsing result.
     */
    private LKQLConfigFileResult getRuleConfigFileResult() {
        if (this.ruleConfigFileResult == null) {
            if (!this.getRuleConfigFile().isEmpty()) {
                this.ruleConfigFileResult =
                        ParsingUtils.parseLKQLConfigFile(this, this.getRuleConfigFile());
            } else {
                this.ruleConfigFileResult =
                        new LKQLConfigFileResult(
                                new ArrayList<>(),
                                new ArrayList<>(),
                                new ArrayList<>(),
                                new HashMap<>(),
                                new HashMap<>());
            }
        }
        return this.ruleConfigFileResult;
    }

    /**
     * Get rules to apply to all codes.
     *
     * @return The list containing rules.
     */
    public List<String> getAllRules() {
        if (this.allRules == null) {
            this.allRules = new ArrayList<>();
            this.allRules.addAll(this.getRuleConfigFileResult().allRules());
            this.allRules.addAll(List.of(this.getSpecifiedRules()));
        }
        return this.allRules;
    }

    /**
     * Get rules to apply to Ada code only.
     *
     * @return The list containing rules.
     */
    public List<String> getAdaRules() {
        if (this.adaRules == null) {
            this.adaRules = new ArrayList<>();
            this.adaRules.addAll(this.getRuleConfigFileResult().adaRules());
        }
        return this.adaRules;
    }

    /**
     * Get the rules to apply to SPARK code only.
     *
     * @return The list containing the rules.
     */
    public List<String> getSparkRules() {
        if (this.sparkRules == null) {
            this.sparkRules = new ArrayList<>();
            this.sparkRules.addAll(this.getRuleConfigFileResult().sparkRules());
        }
        return this.sparkRules;
    }

    /**
     * Get the rule name corresponding to the given alias.
     *
     * @param alias The alias to get the rule from.
     * @return The rule name if the alias corresponds to one, null else.
     */
    public String getRuleFromAlias(final String alias) {
        if (this.allAliases == null) {
            this.allAliases = new HashMap<>();
            this.allAliases.putAll(this.getRuleConfigFileResult().aliases());
        }
        return this.allAliases.get(alias);
    }

    /**
     * Get the argument value for the wanted rule.
     *
     * @param ruleName The name of the rule to get the arguments for.
     * @param argName The argument name to get.
     * @return The value of the argument for the rule or null.
     */
    @CompilerDirectives.TruffleBoundary
    public Object getRuleArg(String ruleName, String argName) {
        if (this.allRulesArgs == null) {
            this.allRulesArgs = new HashMap<>();
            this.allRulesArgs.putAll(this.getRuleConfigFileResult().args());
            this.allRulesArgs.putAll(
                    ParsingUtils.parseRulesArgs(
                            this.getEnv().getOptions().get(LKQLLanguage.rulesArgs).split(";")));
        }
        Map<String, Object> ruleArgs = this.allRulesArgs.getOrDefault(ruleName, null);
        return ruleArgs == null ? null : ruleArgs.getOrDefault(argName, null);
    }

    /**
     * Get the filtered node rules in this context.
     *
     * @return The node checkers array filtered according to options.
     */
    @CompilerDirectives.TruffleBoundary
    public NodeChecker[] getAllNodeCheckers() {
        if (this.filteredAllNodeCheckers == null) {
            this.initCheckerCaches();
        }
        return this.filteredAllNodeCheckers;
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
        final List<NodeChecker> allNodeCheckers = new ArrayList<>();
        final List<NodeChecker> adaNodeCheckers = new ArrayList<>();
        final List<NodeChecker> sparkNodeCheckers = new ArrayList<>();
        final List<UnitChecker> unitCheckers = new ArrayList<>();
        final Map<String, BaseChecker> allCheckers = this.global.getCheckers();

        // Get the command line required rules
        final List<String> allRules = this.getAllRules();
        final List<String> adaRules = this.getAdaRules();
        final List<String> sparkRules = this.getSparkRules();

        // Lambda to dispatch checkers in the correct lists
        final BiConsumer<BaseChecker, List<NodeChecker>> dispatchChecker =
                (checker, nodeCheckers) -> {
                    if (checker instanceof NodeChecker nodeChecker) {
                        nodeCheckers.add(nodeChecker);
                        if (nodeChecker.isFollowGenericInstantiations()) {
                            needsToFollowInstantiations = true;
                        }
                    } else {
                        UnitChecker unitChecker = (UnitChecker) checker;
                        unitCheckers.add(unitChecker);
                    }
                };

        // Lambda to get the checker object value from the rule name
        final Function<String, BaseChecker> getAssociatedChecker =
                (ruleName) -> {
                    // Get the checker from the given rule name
                    BaseChecker checker;
                    final String aliasResolved = this.getRuleFromAlias(ruleName);
                    if (aliasResolved != null) {
                        checker = allCheckers.get(aliasResolved).copy();
                        checker.setAlias(ruleName);
                    } else {
                        checker = allCheckers.get(ruleName);
                    }

                    // Verify that the checker is not null
                    if (checker == null) {
                        throw LKQLRuntimeException.fromMessage(
                                "Could not find any rule named " + ruleName);
                    }

                    // Return the result
                    return checker;
                };

        // If there is no wanted rule, run them all (if the appropriate option is set)
        if (allRules.size() == 0 && this.env.getOptions().get(LKQLLanguage.fallbackToAllRules)) {
            for (BaseChecker checker : allCheckers.values()) {
                dispatchChecker.accept(checker, allNodeCheckers);
            }
        }

        // Else verify and add the wanted rules
        else {
            for (String ruleName : allRules) {
                dispatchChecker.accept(getAssociatedChecker.apply(ruleName), allNodeCheckers);
            }
            for (String ruleName : adaRules) {
                dispatchChecker.accept(getAssociatedChecker.apply(ruleName), adaNodeCheckers);
            }
            for (String ruleName : sparkRules) {
                dispatchChecker.accept(getAssociatedChecker.apply(ruleName), sparkNodeCheckers);
            }
        }

        // Set the checker caches
        this.filteredAllNodeCheckers = allNodeCheckers.toArray(new NodeChecker[0]);
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
