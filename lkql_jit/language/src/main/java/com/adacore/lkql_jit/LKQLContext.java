/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.declarations.functions.FunDecl;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.util_functions.ArrayUtils;
import com.adacore.lkql_jit.utils.util_functions.CheckerUtils;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.*;
import java.util.*;
import java.util.function.Consumer;


/**
 * This class represents the execution context of an LKQL script
 *
 * @author Hugo GUERRIER
 */
public final class LKQLContext {

    // ----- Attributes -----

    /**
     * Environment of the language
     */
    @CompilerDirectives.CompilationFinal
    private TruffleLanguage.Env env;

    /**
     * The global values of the LKQL execution
     */
    private final GlobalScope globalValues;

    // ----- Ada project attributes -----

    /**
     * The analysis context for the ada files
     */
    private Libadalang.AnalysisContext adaContext;

    /**
     * The project manager for the ada project
     */
    private Libadalang.ProjectManager projectManager;

    /**
     * The user-specified source files to analyze. If not explicitly specified, those will be the source files
     * of the root project.
     */
    private List<String> specifiedSourceFiles;

    /**
     * All the source files of the project, including those of its non-externally-built dependencies
     */
    private List<String> allSourceFiles;

    /**
     * If the source files were parsed
     */
    private boolean parsed;

    /**
     * The user-specified units to analyze. If not explicitly specified, those will be the units
     * of the root project.
     */
    private Libadalang.AnalysisUnit[] specifiedUnits;

    /**
     * All the units of the project, including those of its non-externally-built dependencies
     */
    private Libadalang.AnalysisUnit[] allUnits;

    /**
     * The root nodes of all the analysis units of the project.
     */
    private Libadalang.AdaNode[] allUnitsRoots;

    // ----- Checker attributes -----

    /**
     * The rule arguments
     */
    private final Map<String, Map<String, Object>> rulesArgs;

    /**
     * The filtered not checkers cache
     */
    private ObjectValue[] filteredNodeCheckers = null;

    /**
     * The filtered unit checkers cache
     */
    private ObjectValue[] filteredUnitCheckers = null;

    /**
     * Whether there is at least one rule that needs to follow generic instantiations
     */
    private boolean needsToFollowInstantiations = false;

    // ----- Option caches -----

    /**
     * If the language is in the verbose mode
     */
    @CompilerDirectives.CompilationFinal
    private Boolean isVerbose = null;

    /**
     * If the language is in the checker mode
     */
    @CompilerDirectives.CompilationFinal
    private Boolean isChecker = null;

    @CompilerDirectives.CompilationFinal
    private Boolean keepGoingOnMissingFile = null;

    /**
     * The project file to analyse
     */
    @CompilerDirectives.CompilationFinal
    private String projectFile = null;

    /**
     * The project's scenario variables
     */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private Libadalang.ScenarioVariable[] scenarioVars = null;

    /**
     * The ada files passed through the command line
     */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] files = null;

    /**
     * The error printing mode
     */
    @CompilerDirectives.CompilationFinal
    private String errorMode = null;

    /**
     * Whether the checker is in debug mode
     */
    @CompilerDirectives.CompilationFinal
    private Boolean checkerDebug = null;

    /**
     * The rle to execute
     */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] rules;

    /**
     * The directories where the rule files are located
     */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] rulesDirs;

    /**
     * The files to ignore during an analysis
     */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] ignores;

    @CompilerDirectives.CompilationFinal
    private CheckerUtils.DiagnosticEmitter emitter;

    // ----- Constructors -----

    /**
     * Create a new LKQL context
     *
     * @param env          The environment
     * @param globalValues The initialized global values
     */
    public LKQLContext(
        TruffleLanguage.Env env,
        GlobalScope globalValues
    ) {
        this.env = env;
        this.globalValues = globalValues;
        this.specifiedSourceFiles = new ArrayList<>();
        this.allSourceFiles = new ArrayList<>();
        this.rulesArgs = new HashMap<>();
        this.parsed = false;
    }

    // ----- Destructors -----

    /**
     * Finalize the LKQL context to close libadalang context
     */
    public void finalizeContext() {
        this.adaContext.close();
        if (this.projectManager != null) this.projectManager.close();
    }

    // ----- Getters -----

    public TruffleLanguage.Env getEnv() {
        return this.env;
    }

    public GlobalScope getGlobalValues() {
        return this.globalValues;
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

    public boolean isRootContext() {
        return this.globalValues.getStackSize() == 0;
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
     * Get if the language execution is in verbose mode
     *
     * @return True if the verbose flag is on
     */
    public boolean isVerbose() {
        if (this.isVerbose == null) {
            this.isVerbose = this.env.getOptions().get(LKQLLanguage.verbose);
        }
        return this.isVerbose;
    }

    /**
     * Get if the language is in checker mode
     *
     * @return True if the language is in checher mode
     */
    public boolean isChecker() {
        if (this.isChecker == null) {
            this.isChecker = this.env.getOptions().get(LKQLLanguage.checkerMode);
        }
        return this.isChecker;
    }

    /**
     * Return true if the engine should keep running when a required file is not found.
     */
    public boolean keepGoingOnMissingFile() {
        if (this.keepGoingOnMissingFile == null) {
            this.keepGoingOnMissingFile = this.env.getOptions().get(LKQLLanguage.keepGoingOnMissingFile);
        }
        return this.keepGoingOnMissingFile;
    }

    /**
     * Return the project file of the language context
     *
     * @return The project file in a string
     */
    public String getProjectFile() {
        if (this.projectFile == null) {
            this.projectFile = this.env.getOptions().get(LKQLLanguage.projectFile);
        }
        return this.projectFile;
    }

    /**
     * Return the list of scenario variables to specify when loading the GPR project file.
     */
    public Libadalang.ScenarioVariable[] getScenarioVars() {
        if (this.scenarioVars == null) {
            // Scenario variables are passed as semicolon-separated substrings encoded in Base64.
            String[] bindings = this.env.getOptions().get(LKQLLanguage.scenarioVars).split(";");
            if (bindings.length == 1 && bindings[0].length() == 0) {
                // No scenario variables were specified
                this.scenarioVars = new Libadalang.ScenarioVariable[0];
            } else {
                // Some scenario variables were specified. Decode them from Base64 and parse the `key=value`
                // specification.
                Base64.Decoder decoder = Base64.getDecoder();
                this.scenarioVars = new Libadalang.ScenarioVariable[bindings.length];
                for (int i = 0; i < bindings.length; ++i) {
                    String binding = new String(decoder.decode(bindings[i]));
                    int eqIndex = binding.indexOf('=');
                    if (eqIndex == -1) {
                        throw LKQLRuntimeException.fromMessage("Invalid scenario variable specification: " + binding);
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
     * Get the files to analyse
     *
     * @return The files to analyse in an array
     */
    public String[] getFiles() {
        if (this.files == null) {
            this.files = StringUtils.splitPaths(this.env.getOptions().get(LKQLLanguage.files));
        }
        return this.files;
    }

    /**
     * Get the error handling mode
     *
     * @return The mode in a string
     */
    public String getErrorMode() {
        if (this.errorMode == null) {
            this.errorMode = this.env.getOptions().get(LKQLLanguage.errorMode);
        }
        return this.errorMode;
    }

    /**
     * Get whether the checker is in debug mode
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
     * Get the rule to run with the checker
     *
     * @return The rule to run
     */
    @CompilerDirectives.TruffleBoundary
    private String[] getRules() {
        if (this.rules == null) {
            String[] unfilteredRules = this.env.getOptions().get(LKQLLanguage.rules).trim().replace(" ", "").split(",");
            this.rules = Arrays.stream(unfilteredRules)
                .filter(s -> !s.isBlank() && !s.isEmpty())
                .map(String::toLowerCase)
                .distinct()
                .toArray(String[]::new);
        }
        return this.rules;
    }

    /**
     * Get the directories to get the rules from
     *
     * @return The directory array
     */
    public String[] getRulesDirs() {
        if (this.rulesDirs == null) {
            this.rulesDirs = StringUtils.splitPaths(this.env.getOptions().get(LKQLLanguage.rulesDirs));
            String additionalRulesDirs = System.getenv(Constants.LKQL_RULES_PATH);
            if (additionalRulesDirs != null) {
                this.rulesDirs = ArrayUtils.concat(this.rulesDirs, StringUtils.splitPaths(additionalRulesDirs));
            }
        }
        return this.rulesDirs;
    }

    /**
     * Get the Ada file to ignore during the analysis
     *
     * @return The array containing all Ada files to ignore
     */
    public String[] getIgnores() {
        if (this.ignores == null) {
            this.ignores = StringUtils.splitPaths(this.env.getOptions().get(LKQLLanguage.ignores));
            this.ignores = Arrays.stream(this.ignores)
                .filter(s -> !s.isBlank() && !s.isEmpty())
                .toArray(String[]::new);
        }
        return this.ignores;
    }

    /**
     * Invalidate the option caches
     */
    private void invalidateOptionCaches() {
        this.isVerbose = null;
        this.isChecker = null;
        this.projectFile = null;
        this.files = null;
        this.errorMode = null;
        this.rules = null;
        this.rulesDirs = null;
        this.ignores = null;
        this.emitter = null;
    }

    // ----- Value related methods -----

    /**
     * Get the value of a global symbol
     *
     * @param slot The slot to get
     * @return The value in the global scope
     */
    public Object getGlobal(int slot) {
        return this.globalValues.get(slot);
    }

    /**
     * Set a global value in the context
     *
     * @param slot   The slot of the variable
     * @param symbol The value symbol
     * @param value  The value
     */
    public void setGlobal(int slot, String symbol, Object value) {
        this.globalValues.set(slot, symbol, value);
    }

    /**
     * Get the meta table for the given type
     *
     * @param type The type to get the meta table for
     * @return The meta table for the type
     */
    public Map<String, BuiltInFunctionValue> getMetaTable(String type) {
        return this.globalValues.getMetaTable(type);
    }

    // ----- IO methods -----

    /**
     * Display the given string
     *
     * @param toPrint The string to print
     */
    @CompilerDirectives.TruffleBoundary
    public void print(String toPrint) {
        System.out.print(toPrint);
    }

    /**
     * Display the given string with a newline
     *
     * @param toPrint The string to print
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
            this.emitter = switch (this.env.getOptions().get(LKQLLanguage.diagnosticOutputMode)) {
                case PRETTY -> new CheckerUtils.DefaultEmitter();
                case GNATCHECK -> new CheckerUtils.GNATcheckEmitter();
            };
        }
        return this.emitter;
    }

    // ----- Project analysis methods -----

    /**
     * Initialize the ada sources
     */
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
                    System.err.println("Source file '" + file + "' not found");
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
        final Libadalang.EventHandler eventHandler = Libadalang.EventHandler.create(unitRequested, null);

        // If the option is the empty string, the language implementation will end up setting it to the default
        // value for its language (e.g. iso-8859-1 for Ada).
        String charset = this.env.getOptions().get(LKQLLanguage.charset);

        // Get the project file and parse it if there is one
        String projectFileName = this.getProjectFile();

        if (projectFileName != null && !projectFileName.isEmpty() && !projectFileName.isBlank()) {
            // Create the project manager
            this.projectManager = Libadalang.ProjectManager.create(projectFileName, this.getScenarioVars(), "", "");

            final String subprojectName = this.env.getOptions().get(LKQLLanguage.subprojectFile);
            final String[] subprojects = subprojectName.isEmpty() ? null : new String[] {subprojectName};

            // If no files were specified by the user, the files to analyze are those of the root project
            // (i.e. without recusing into project dependencies)
            if (this.specifiedSourceFiles.isEmpty()) {
                this.specifiedSourceFiles = Arrays.stream(
                    this.projectManager.getFiles(Libadalang.SourceFileMode.ROOT_PROJECT, subprojects)
                ).toList();
            }

            // The `units()` built-in function must return all units of the project including units from its
            // dependencies. So let's retrieve all those files as well.
            this.allSourceFiles = Arrays.stream(
                this.projectManager.getFiles(Libadalang.SourceFileMode.WHOLE_PROJECT, subprojects)
            ).toList();

            this.adaContext = this.projectManager.createContext(
                subprojectName.isEmpty() ? null : subprojectName,
                eventHandler,
                true,
                8
            );
        } else {
            // When no project is specified, `units()` should return the same set of units as `specified_units()`.
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
                System.err.println("Scenario variable specifications require a project file");
            }

            this.adaContext = Libadalang.AnalysisContext.create(
                charset,
                null,
                provider,
                eventHandler,
                true,
                8
            );

            // In the absence of a project file, we consider for now that there are no configuration pragmas.
            this.adaContext.setConfigPragmasMapping(null, null);
        }

        eventHandler.close();

        // The retrieved source files are not yet parsed
        this.parsed = false;
    }

    /**
     * Return the list of files that belong to the available runtime. We only return specification files,
     * as implementation are not useful for resolving names and types in the actual user sources.
     * If a GNAT installation is not available in the PATH, this returns an empty list without error.
     */
    @CompilerDirectives.TruffleBoundary
    public List<String> fetchAdaRuntimeFiles() {
        final List<String> runtimeFiles = new ArrayList<>();
        try {
            final Process gnatls = new ProcessBuilder("gnatls", "-v").start();
            final BufferedReader reader = new BufferedReader(new InputStreamReader(gnatls.getInputStream()));
            final Optional<String> adaIncludePath =
                reader.lines().filter(line -> line.contains("adainclude")).findFirst();
            adaIncludePath.ifPresent(path -> {
                final Path adaIncludeDir = Paths.get(path.trim());
                try (DirectoryStream<Path> stream = Files.newDirectoryStream(adaIncludeDir, "*.ads")) {
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

    /**
     * Parse the ada source files and store analysis units and root nodes
     */
    @CompilerDirectives.TruffleBoundary
    public void parseSources() {
        // Filter the Ada source file list
        String[] ignores = this.getIgnores();
        String[] usedSources = this.specifiedSourceFiles.stream()
            .filter(source -> {
                for (String ignore : ignores) {
                    if (source.contains(ignore)) return false;
                }
                return true;
            })
            .toArray(String[]::new);

        // For each specified source file, store its corresponding analysis unit in the list of specified units
        this.specifiedUnits = new Libadalang.AnalysisUnit[usedSources.length];
        for (int i = 0; i < usedSources.length; i++) {
            this.specifiedUnits[i] = this.adaContext.getUnitFromFile(usedSources[i]);
        }

        // For each source file of the project, store its corresponding analysis unit in the list of all the units
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

    // ----- Checker methods -----

    /**
     * Add an argument for a rule execution
     *
     * @param ruleName The rule name
     * @param argName  The argument name
     * @param value    The value of the argument
     */
    @CompilerDirectives.TruffleBoundary
    public void addRuleArg(String ruleName, String argName, Object value) {
        Map<String, Object> args = this.rulesArgs.getOrDefault(ruleName, new HashMap<>());
        args.put(argName, value);
        this.rulesArgs.put(ruleName, args);
    }

    /**
     * Get the argument value for the wanted rule
     *
     * @param ruleName The name of the rule to get the arguments for
     * @param argName  The argument name to get
     * @return The value of the argument for the rule or null
     */
    @CompilerDirectives.TruffleBoundary
    public Object getRuleArg(String ruleName, String argName) {
        Map<String, Object> ruleArgs = this.rulesArgs.getOrDefault(ruleName, null);
        return ruleArgs == null ?
            null :
            ruleArgs.getOrDefault(argName, null);
    }

    /**
     * Initialize the filtered and separated checker caches.
     */
    @CompilerDirectives.TruffleBoundary
    private void initCheckerCaches() {
        // Prepare the working variables
        final List<ObjectValue> nodeCheckers = new ArrayList<>();
        final List<ObjectValue> unitCheckers = new ArrayList<>();
        final Map<String, ObjectValue> allCheckers = this.globalValues.getCheckers();
        final String[] wantedRules = this.getRules();

        // Lambda to dispatch checkers in the correct lists
        final Consumer<ObjectValue> dispatchChecker = (checker) -> {
            if (checker.get("mode") == FunDecl.CheckerMode.NODE) {
                nodeCheckers.add(checker);
                if ((boolean) checker.get("follow_generic_instantiations")) {
                    needsToFollowInstantiations = true;
                }
            } else {
                unitCheckers.add(checker);
            }
        };

        // If there is no wanted rule, run them all (if the appropriate option is set)
        if (wantedRules.length == 0 && this.env.getOptions().get(LKQLLanguage.fallbackToAllRules)) {
            for (ObjectValue checker : allCheckers.values()) {
                dispatchChecker.accept(checker);
            }
        }

        // Else verify and add the wanted rules
        else {
            for (String rule : wantedRules) {
                if (allCheckers.containsKey(rule)) {
                    dispatchChecker.accept(allCheckers.get(rule));
                } else {
                    throw LKQLRuntimeException.fromMessage("Could not find any rule named " + rule);
                }
            }
        }

        // Set the checker caches
        this.filteredNodeCheckers = nodeCheckers.toArray(new ObjectValue[0]);
        this.filteredUnitCheckers = unitCheckers.toArray(new ObjectValue[0]);
    }

    /**
     * @return whether there is at least one rule that needs to follow generic instantiations
     */
    public boolean mustFollowInstantiations() {
        return needsToFollowInstantiations;
    }

    /**
     * Get the filtered node rules in this context
     *
     * @return The node rule list filtered according to options
     */
    @CompilerDirectives.TruffleBoundary
    public ObjectValue[] getNodeCheckersFiltered() {
        if (this.filteredNodeCheckers == null) {
            this.initCheckerCaches();
        }
        return this.filteredNodeCheckers;
    }

    /**
     * Get the filtered unit checkers for the context
     *
     * @return The list for unit checkers filtered according to options
     */
    @CompilerDirectives.TruffleBoundary
    public ObjectValue[] getUnitCheckersFiltered() {
        if (this.filteredUnitCheckers == null) {
            this.initCheckerCaches();
        }
        return this.filteredUnitCheckers;
    }

}
