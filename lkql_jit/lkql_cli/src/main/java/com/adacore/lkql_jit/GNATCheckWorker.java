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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;
import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import picocli.CommandLine;

/**
 * Implement a worker process for the GNATcheck driver.
 *
 * @author Romain BEGUET
 */
public class GNATCheckWorker extends AbstractLanguageLauncher {

    Args args = null;

    @CommandLine.Command(
            name = "gnatcheck_worker",
            description = "Internal driver meant to be called by GNATcheck. Not for public use")
    public static class Args implements Callable<Integer> {
        @CommandLine.Unmatched public List<String> unmatched;

        @CommandLine.Option(
                names = {"-C", "--charset"},
                description = "Charset to use for the source decoding")
        public String charset = null;

        @CommandLine.Option(names = "--RTS", description = "Runtime to use (not handled yet)")
        public String RTS = null;

        @CommandLine.Option(names = "--target", description = "Runtime to use (not handled yet)")
        public String target = null;

        @CommandLine.Option(
                names = {"-v", "--verbose"},
                description = "Enable the verbose mode")
        public boolean verbose;

        @CommandLine.Option(
                names = {"-P", "--project"},
                description = "Project file to use")
        public String project = null;

        @CommandLine.Option(
                names = "-A",
                description =
                        "The name of the subproject to analyse, if any. This implies that"
                                + " `projectFile` designates an aggregate project.")
        public String subProject = null;

        @CommandLine.Option(names = "-d", description = "Enable the debug mode")
        public boolean debug;

        @CommandLine.Option(names = "-X", description = "Scenario variable")
        public Map<String, String> scenarioVariables = new HashMap<>();

        @CommandLine.Option(names = "--simple-project", description = "Enable simple project mode")
        public boolean isSimpleProject;

        @CommandLine.Option(
                names = "--rules-dir",
                description = "Additional directory in which to check for rules")
        public List<String> rulesDirs = new ArrayList<>();

        @CommandLine.Option(names = "--rules-from", description = "The file containing the rules")
        public String rulesFrom = null;

        @CommandLine.Option(names = "--files-from", description = "The file containing the files")
        public String filesFrom = null;

        @CommandLine.Option(names = "--out-file", description = "The output report file")
        public String outputFile = null;

        @CommandLine.Option(
                names = "--ignore-project-switches",
                description =
                        "Process all units in the project tree, excluding externally built"
                                + " projects")
        public boolean ignore_project_switches;

        @Override
        public Integer call() {
            String[] unmatchedArgs;
            if (this.unmatched == null) {
                unmatchedArgs = new String[0];
            } else {
                unmatchedArgs = this.unmatched.toArray(new String[0]);
            }
            new GNATCheckWorker(this).launch(unmatchedArgs);
            return 0;
        }
    }

    public GNATCheckWorker(GNATCheckWorker.Args args) {
        this.args = args;
    }

    // ----- Macros and enums -----

    /** The identifier of the LKQL language. */
    private static final String ID = "lkql";

    // ----- Checker methods -----

    /**
     * Display the help message for the LKQL language.
     *
     * @param maxCategory The option category.
     */
    @Override
    protected void printHelp(OptionCategory maxCategory) {
        System.out.println("No help!");
    }

    /**
     * Simply return the language id.
     *
     * @return The language id.
     */
    @Override
    protected String getLanguageId() {
        return ID;
    }

    /**
     * Start the GNATcheck worker.
     *
     * @param contextBuilder The context builder to build LKQL context.
     */
    @Override
    protected void launch(Context.Builder contextBuilder) {
        int exitCode = this.executeScript(contextBuilder);
        if (exitCode != 0) {
            throw this.abort((String) null, exitCode);
        }
    }

    /**
     * Execute the GNATcheck worker script and return the exit code.
     *
     * @param contextBuilder The context builder.
     * @return The exit code of the script.
     */
    protected int executeScript(Context.Builder contextBuilder) {
        // Set the builder common options
        contextBuilder.allowIO(true);
        contextBuilder.option("lkql.diagnosticOutputMode", "GNATCHECK");

        // If no rules are provided, don't do anything
        contextBuilder.option("lkql.fallbackToAllRules", "false");

        // Do not stop the worker's execution when a source file is missing
        contextBuilder.option("lkql.keepGoingOnMissingFile", "true");

        // Set the context options
        if (this.args.verbose) {
            contextBuilder.option("lkql.verbose", "true");
        }

        // Set the project file
        if (this.args.isSimpleProject) {
            contextBuilder.option("lkql.useAutoProvider", "true");
            // TODO: Add warning message since auto provider and project file are apparently
            //  mutually exclusive
        } else {
            if (this.args.project != null) {
                contextBuilder.option("lkql.projectFile", this.args.project);
            }

            if (this.args.subProject != null) {
                contextBuilder.option("lkql.subprojectFile", this.args.subProject);
            }
        }

        if (this.args.debug) {
            contextBuilder.option("lkql.checkerDebug", "true");
        }

        if (!this.args.scenarioVariables.isEmpty()) {
            StringBuilder scenarioVars = new StringBuilder();
            Base64.Encoder encoder = Base64.getEncoder();
            this.args.scenarioVariables.forEach(
                    (key, val) -> {
                        scenarioVars.append(
                                new String(encoder.encode((key + "=" + val).getBytes())));
                        scenarioVars.append(";");
                    });
            contextBuilder.option("lkql.scenarioVars", scenarioVars.toString());
        }

        // Set the files
        if (this.args.filesFrom != null) {
            try {
                final List<String> lines = Files.readAllLines(Paths.get(this.args.filesFrom));
                final String files = String.join(File.pathSeparator, lines);
                contextBuilder.option("lkql.files", files);
            } catch (IOException e) {
                System.err.println("Could not read file: " + this.args.filesFrom);
            }
        }

        // Set the charset
        if (this.args.charset != null
                && !this.args.charset.isEmpty()
                && !this.args.charset.isBlank()) {
            contextBuilder.option("lkql.charset", this.args.charset);
        }

        // Set the rule directories
        if (!this.args.rulesDirs.isEmpty()) {
            contextBuilder.option(
                    "lkql.rulesDirs", String.join(File.pathSeparator, this.args.rulesDirs));
        }

        // Set the rule to apply
        if (!this.args.rulesFrom.isEmpty()) {
            if (this.args.rulesFrom.endsWith(".lkql")) {
                contextBuilder.option("lkql.LKQLRuleFile", this.args.rulesFrom);
            } else {
                final List<String> allRules = new ArrayList<>();
                final List<String> allArgs = new ArrayList<>();
                processRuleSpecificationFile(this.args.rulesFrom, allRules, allArgs);
                contextBuilder.option("lkql.rules", String.join(",", allRules));
                contextBuilder.option("lkql.rulesArgs", String.join(";", allArgs));
            }
        }

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            final Source source = Source.newBuilder("lkql", checkerSource, "checker.lkql").build();
            final Value executable = context.parse(source);
            executable.executeVoid(true);
            return 0;
        } catch (Exception e) {
            System.out.println("WORKER_FATAL_ERROR: " + e.getMessage());
            return 0;
        }
    }

    /**
     * Parse the given GNATcheck rule specification file and fill in the list of rules and rule
     * arguments accordingly.
     *
     * <p>The file is expected to have one rule specification per line.
     *
     * @param filename The filename containing the rule specifications for this run.
     * @param allRules The list in which to add all parsed rules.
     * @param allArgs The list in which to add all parsed rule arguments.
     */
    private static void processRuleSpecificationFile(
            String filename, List<String> allRules, List<String> allArgs) {
        try {
            for (String ruleSpec : Files.readAllLines(Paths.get(filename))) {
                processRuleSpecification(ruleSpec, allRules, allArgs);
            }
        } catch (IOException e) {
            System.err.println("Could not read file: " + filename);
        }
    }

    /**
     * Parse the given GNATcheck rule specification and add it to the list of rules to run.
     *
     * <p>A GNATcheck rule specification can also specify arguments for that rule, so these are
     * parsed as well and added to the list of rule arguments.
     *
     * @param ruleSpec The rule specification to parse.
     * @param allRules The list of all currently parsed rules, in which we'll add this one.
     * @param allArgs The list of all currently specified rule arguments, which might be expanded
     *     here.
     */
    private static void processRuleSpecification(
            String ruleSpec, List<String> allRules, List<String> allArgs) {
        if (ruleSpec.startsWith("-")) {
            String ruleName = allRules.get(allRules.size() - 1);
            String arg = ruleSpec.substring(1);
            allArgs.add(ruleName + "." + arg);
        } else {
            allRules.add(ruleSpec);
        }
    }

    protected List<String> preprocessArguments(
            List<String> arguments, Map<String, String> polyglotOptions) {
        if (this.args.unmatched != null) {
            return this.args.unmatched;
        } else {
            return new ArrayList<String>();
        }
    }

    // ----- The LKQL checker -----

    public static final String checkerSource =
            """
            val analysis_units = specified_units()
            val roots = [unit.root for unit in analysis_units]

            map(roots, (root) => node_checker(root))
            map(analysis_units, (unit) => unit_checker(unit))
            """;
}
