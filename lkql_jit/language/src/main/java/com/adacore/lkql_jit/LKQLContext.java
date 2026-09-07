//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.exceptions.LogLocation;
import com.adacore.lkql_jit.langkit_translator.passes.Hierarchy;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLogger;
import java.io.File;
import java.nio.file.Paths;
import java.util.*;
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

    /** The stack representing the current LKQL source chain. */
    public final Stack<String> fromStack = new Stack<>();

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
                this.getLogger().log(
                    missingFileIsError() ? Level.SEVERE : Level.WARNING,
                    "File " + adaFileName + " not found",
                    new LogLocation(
                        new LogLocation.LangkitLocation(
                            from,
                            reportLocationNode.getSourceLocationRange()
                        )
                    )
                );
            }
        },
        null,
        (ctx, unit, message) -> {
            this.getLogger().log(
                Level.SEVERE,
                message,
                new LogLocation(
                    new LogLocation.LangkitLocation(unit, unit.getRoot().getSourceLocationRange())
                )
            );
        }
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
    private final List<String> allSourceFiles;

    // ----- Option caches -----

    /** Options object passed to the LKQL engine. */
    @CompilerDirectives.CompilationFinal
    private LKQLOptions options = null;

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

    @CompilerDirectives.TruffleBoundary
    public boolean isSourceInStack(String sourceName) {
        return fromStack.contains(sourceName);
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

    @CompilerDirectives.TruffleBoundary
    public void pushSourceToStack(String sourceName) {
        fromStack.push(sourceName);
    }

    @CompilerDirectives.TruffleBoundary
    public void popSourceFromStack() {
        fromStack.pop();
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

    /** Return true if the engine should keep running when a required file is not found. */
    public boolean missingFileIsError() {
        return this.getOptions().missingFileIsError();
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
     * Get the Ada file to ignore during the analysis.
     *
     * @return The array containing all Ada files to ignore.
     */
    public String[] getIgnores() {
        return this.getOptions().ignores().toArray(new String[0]);
    }

    public List<String> getAdditionalLkqlPaths() {
        return this.getOptions().additionalLkqlPaths();
    }

    /** Get whether project diagnostics should be hidden. */
    public boolean hideProjectDiagnostics() {
        return this.getOptions().hideProjectDiagnostics();
    }

    /** Invalidate the option caches. */
    private void invalidateOptionCaches() {
        this.options = null;
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

    /** Initialize the Ada sources. */
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
            if (
                !hideProjectDiagnostics() &&
                !diagnosticsToForward.isEmpty() &&
                loadedProject.isPresent()
            ) {
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
}
