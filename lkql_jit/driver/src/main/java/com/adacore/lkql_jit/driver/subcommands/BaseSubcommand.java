//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.DiagnosticLogHandler;
import com.adacore.lkql_jit.driver.source_support.SourceLinesCache;
import com.adacore.lkql_jit.options.LKQLOptions;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.Callable;
import org.graalvm.launcher.AbstractLanguageLauncher;
import picocli.CommandLine;

public abstract class BaseSubcommand extends AbstractLanguageLauncher implements Callable<Integer> {

    // ----- Attributes -----

    /** Whether the current output support ANSI colors. */
    protected final boolean supportAnsi;

    /** Provide support for diagnostic collection in sub-commands. */
    protected final DiagnosticCollector diagnostics;

    /** Handle log from the Truffle engine. */
    protected final DiagnosticLogHandler logHandler;

    /** Object used to cache sources lines from files when fetching their content. */
    protected final SourceLinesCache linesCache;

    // ----- Constructors -----

    protected BaseSubcommand() {
        this.supportAnsi = System.getenv("TERM") != null && System.console() != null;
        this.diagnostics = new DiagnosticCollector();
        this.linesCache = new SourceLinesCache();
        this.logHandler = new DiagnosticLogHandler(diagnostics, linesCache);
    }

    // ----- Inner classes -----

    /** This class defines all CLI arguments dedicated to GPR related argument parsing. */
    public static class GPRArgs {

        @CommandLine.Option(
            names = { "-aP" },
            description = "Add <directory> to the project search path",
            paramLabel = "<directory>"
        )
        public List<Path> ap = new ArrayList<>();

        @CommandLine.Option(
            names = { "--autoconf" },
            description = "Specify/create the main config project file name",
            paramLabel = "<file>"
        )
        public Path autoconf;

        @CommandLine.Option(
            names = { "--config" },
            description = "Specify the configuration project file name",
            paramLabel = "<file>"
        )
        public Path config;

        @CommandLine.Option(
            names = { "--db" },
            description = "Parse directory as an additional knowledge base",
            paramLabel = "<directory>"
        )
        public List<Path> db = new ArrayList<>();

        @CommandLine.Option(
            names = { "--db-" },
            description = "Do not load the standard knowledge base"
        )
        public boolean dbMinus;

        @CommandLine.Option(
            names = { "--implicit-with" },
            description = "Add the given projects as a dependency on all loaded projects",
            paramLabel = "<project>"
        )
        public List<String> implicitWith = new ArrayList<>();

        @CommandLine.Option(names = { "-eL" }, description = "Follows symlinks for project files")
        public boolean resolveLinks;

        @CommandLine.Option(names = { "--no-project" }, description = "Do not use a project file")
        public boolean noProject;

        @CommandLine.Option(
            names = { "-P" },
            description = "The project file",
            paramLabel = "<project>"
        )
        public String p;

        @CommandLine.Option(
            names = { "--root-dir" },
            description = "Root directory of obj/lib/exec to relocate",
            paramLabel = "<directory>"
        )
        public Path rootDir;

        @CommandLine.Option(
            names = { "--relocate-build-tree" },
            description = "Root obj/lib/exec dirs are current-directory or directory",
            paramLabel = "<directory>"
        )
        public Path relocateBuildTree;

        @CommandLine.Option(
            names = { "--RTS" },
            description = "Specify the Ada runtime",
            paramLabel = "<runtime>"
        )
        public String adaRts;

        @CommandLine.Option(
            names = { "--RTS:" },
            description = "Use --RTS:<lang>=<runtime> to specify the runtime for language <lang>",
            paramLabel = "<lang=runtime>"
        )
        public Map<String, String> rts = new HashMap<>();

        @CommandLine.Option(
            names = { "--src-subdirs" },
            description = "prepend <obj>/directory to the list of source dirs for each project",
            paramLabel = "<directory>"
        )
        public Path srcSubdirs;

        @CommandLine.Option(
            names = { "--subdirs" },
            description = "Use directory as suffix to obj/lib/exec directories",
            paramLabel = "<directory>"
        )
        public Path subdirs;

        @CommandLine.Option(
            names = { "--target" },
            description = "Specify a target for cross platforms"
        )
        public String target;

        @CommandLine.Option(
            names = { "-X" },
            description = "Specify an external reference for Project Files",
            paramLabel = "<name=value>"
        )
        public Map<String, String> x;

        /** Fill the provided options builder with GPR CLI arguments parsed by this instance. */
        public void fillGPROptions(LKQLOptions.Builder optionsBuilder) {
            optionsBuilder
                .additionalProjectPaths(ap.stream().map(Path::toString).toList())
                .autoconf(autoconf == null ? null : autoconf.toString())
                .configFile(config == null ? null : config.toString())
                .additionalKnowledgeBases(db.stream().map(Path::toString).toList())
                .skipStandardKnowledgeBase(dbMinus)
                .implicitWiths(implicitWith)
                .followSymlinks(resolveLinks)
                .noProject(noProject)
                .projectFile(p)
                .rootDir(rootDir == null ? null : rootDir.toString())
                .relocateBuildTree(relocateBuildTree == null ? null : relocateBuildTree.toString())
                .adaRuntime(adaRts)
                .runtimes(rts)
                .srcSubdirs(srcSubdirs == null ? null : srcSubdirs.toString())
                .subdirs(subdirs == null ? null : subdirs.toString())
                .target(target)
                .scenarioVariables(x);
        }
    }
}
