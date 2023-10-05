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
import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

/**
 * Implement a worker process for the GNATcheck driver.
 *
 * @author Romain BEGUET
 */
public class GNATCheckWorker extends AbstractLanguageLauncher {

    // ----- Macros and enums -----

    /** The identifier of the LKQL language. */
    private static final String ID = "lkql";

    // ----- Launcher options -----

    /** The charset to decode the LKQL sources. */
    private String charset = null;

    /** If the project analysis should be recursive. */
    private boolean recursive = false;

    /** If the verbose mode should be activated. */
    private boolean verbose = false;

    /** The project file to analyse. */
    private String projectFile = null;

    /**
     * The name of the subproject to analyse, if any. This implies that `projectFile` designates an
     * aggregate project.
     */
    private String subprojectFile = null;

    /** Whether GNATcheck is in debug mode. */
    private boolean debug = false;

    /** The project's scenario variables. */
    private String scenarioVars = null;

    /** Whether the '--simple-project' flag was used. */
    private boolean isSimpleProject = false;

    /** A directory containing all user added rules. */
    private String rulesDirs = null;

    /** The rules to apply. */
    private String rulesFrom = null;

    /** The LKQL file to configure the rules. */
    private String rulesFromLKQL = null;

    /** The file containing the files to analyse. */
    private String filesFrom = null;

    /** The source files to ignore during analysis. */
    private String ignore = null;

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
     * Start the LKQL checker.
     *
     * @param args The params.
     */
    public static void main(String[] args) {
        new GNATCheckWorker().launch(args);
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
        if (this.verbose) {
            contextBuilder.option("lkql.verbose", "true");
        }

        // Set the project file
        if (this.projectFile != null) {
            contextBuilder.option("lkql.projectFile", this.projectFile);
        }

        if (this.subprojectFile != null) {
            contextBuilder.option("lkql.subprojectFile", this.subprojectFile);
        }

        if (this.debug) {
            contextBuilder.option("lkql.checkerDebug", "true");
        }

        if (this.scenarioVars != null) {
            contextBuilder.option("lkql.scenarioVars", this.scenarioVars);
        }

        if (this.isSimpleProject) {
            contextBuilder.option("lkql.useAutoProvider", "true");
        }

        // Set the files
        if (!this.filesFrom.isEmpty()) {
            try {
                final List<String> lines = Files.readAllLines(Paths.get(this.filesFrom));
                final String files = String.join(File.pathSeparator, lines);
                contextBuilder.option("lkql.files", files);
            } catch (IOException e) {
                System.err.println("Could not read file: " + this.filesFrom);
            }
        }

        // Set the charset
        if (this.charset != null && !this.charset.isEmpty() && !this.charset.isBlank()) {
            contextBuilder.option("lkql.charset", this.charset);
        }

        // Set the rule directories
        if (this.rulesDirs != null) {
            contextBuilder.option("lkql.rulesDirs", this.rulesDirs);
        }

        // Set the rule to apply
        if (!this.rulesFrom.isEmpty()) {
            final List<String> allRules = new ArrayList<>();
            final List<String> allArgs = new ArrayList<>();
            processRuleSpecificationFile(this.rulesFrom, allRules, allArgs);
            contextBuilder.option("lkql.rules", String.join(",", allRules));
            contextBuilder.option("lkql.rulesArgs", String.join(";", allArgs));
        }

        // Set the LKQL rule config file
        if (this.rulesFromLKQL != null && !this.rulesFromLKQL.isEmpty()) {
            contextBuilder.option("lkql.LKQLRuleFile", this.rulesFromLKQL);
        }

        if (this.ignore != null && !this.ignore.isEmpty() && !this.ignore.isBlank()) {
            contextBuilder.option("lkql.ignores", this.ignore);
        }

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            final Source source = Source.newBuilder("lkql", checkerSource, "checker.lkql").build();
            final Value executable = context.parse(source);
            executable.executeVoid(true);
            return 0;
        } catch (Exception e) {
            if (this.verbose) {
                e.printStackTrace();
            } else {
                System.err.println(e.getMessage());
            }
            return 0;
        }
    }

    // ----- Argument parsing methods -----

    /**
     * Get the GNATcheck worker specific argument and return the unparsed ones to the default
     * parser.
     *
     * <p>The expected format for command-line arguments is as follows (the order should be
     * respected):
     *
     * <ul>
     *   <li>`--subprocess` No-op here. Originally used by GNATcheck to discriminate between a
     *       driver run and a worker run, since they were implemented by the same application.
     *   <li>`[-P{project} [--ignore-project-switches]]` The project file to use, and whether to
     *       ignore GNATcheck- related options that are specified in it.
     *   <li>`[--simple-project]`
     *   <li>`[-A]` The aggregated project to consider, used when the main project file is an
     *       aggregate project.
     *   <li>`[--RTS {runtime}]` The runtime to use
     *   <li>`[--target {target}]` The target to use
     *   <li>`[-d]` Whether to output debug information
     *   <li>`[-eL]`
     *   <li>`[--no_objects_dir]` Whether to output data in the objects directory. TODO: Not sure if
     *       relevant for workers
     *   <li>`[--rules-dir {rule_dir}]*` Additional custom directories in which to search for the
     *       specified rules
     *   <li>`[-U]` Whether to consider project sources recursively. Not relevant for worker, as
     *       files to consider are explicitly given by the driver.
     *   <li>`-files {path}` A path to a temporary file generated by the driver, which contains the
     *       list of unit names that this worker should be processing during its run.
     *   <li>`[-Xvar=val]*` The GPR scenario variables to use when loading the project file
     *   <li>`-rules` No-op.
     *   <li>`-from {path}` A path to a temporary file generated by the driver, which contains the
     *       list of rule specifications (to be parsed) that this worker should use during its run
     *   <li>`-from-lkql {path}` A path to an LKQL file which contains the list of rule
     *       specifications (to be parsed) that this worker should use during its run
     * </ul>
     *
     * @param arguments The arguments to parse.
     * @param polyglotOptions The polyglot options.
     * @return The unrecognized options.
     */
    @Override
    protected List<String> preprocessArguments(
            List<String> arguments, Map<String, String> polyglotOptions) {
        // Iterate over arguments and parse them
        ListIterator<String> iterator = arguments.listIterator();
        String currentArg = iterator.next();

        // First argument should be "--subprocess"

        assert currentArg.equals("--subprocess");
        currentArg = iterator.next();

        // Optional argument: project file
        if (currentArg.startsWith("-P")) {
            this.projectFile = currentArg.substring(2);
            currentArg = iterator.next();

            if (currentArg.equals("--ignore-project-switches")) {
                currentArg = iterator.next();
            }
        }

        // TODO: handle "--simple-project"

        if (currentArg.equals("--simple-project")) {
            currentArg = iterator.next();
            this.projectFile = null;
            this.isSimpleProject = true;
        }

        if (currentArg.equals("-A")) {
            // Use the specified aggregated project (`projectFile` holds the aggregate project)
            this.subprojectFile = iterator.next();

            // TODO: handle "-o=..." and "-ox=..."
            iterator.next();

            currentArg = iterator.next();
        }

        // TODO: handle "--RTS"

        // TODO: handle "--target"

        if (currentArg.equals("-d")) {
            this.debug = true;
            currentArg = iterator.next();
        }

        // TODO: handle "-eL"

        // TODO: handle "--no_objects_dir"

        StringBuilder rulesDirs = new StringBuilder();
        while (currentArg.startsWith("--rules-dir=")) {
            rulesDirs.append(currentArg.substring(12));
            rulesDirs.append(File.pathSeparator);
            currentArg = iterator.next();
        }
        this.rulesDirs = rulesDirs.toString();

        if (currentArg.equals("-U")) {
            this.recursive = true;
            currentArg = iterator.next();
        }

        while (!currentArg.startsWith("-files=")) {
            // ignore all files specified after -U: they are already specified by "-files="
            currentArg = iterator.next();
        }

        assert currentArg.startsWith("-files=");
        this.filesFrom = currentArg.substring(7);
        currentArg = iterator.next();

        // Encode scenario variables specifications (key=value) in Base64 in order to escape `value`
        // which can
        // contain arbitrary characters. Join them with the semicolon character (which cannot appear
        // in
        // a Base64-encoded
        // string).
        StringBuilder scenarioVars = new StringBuilder();
        Base64.Encoder encoder = Base64.getEncoder();
        while (currentArg.startsWith("-X")) {
            String binding = currentArg.substring(2);
            scenarioVars.append(new String(encoder.encode(binding.getBytes())));
            scenarioVars.append(";");
            currentArg = iterator.next();
        }
        this.scenarioVars = scenarioVars.toString();

        // Parse the rules configuration options
        assert currentArg.equals("-rules");

        while (iterator.hasNext()) {
            currentArg = iterator.next();
            if (currentArg.startsWith("-from=")) {
                this.rulesFrom = currentArg.substring(6);
            } else if (currentArg.startsWith("-from-lkql=")) {
                this.rulesFromLKQL = currentArg.substring(11);
            }
        }

        final List<String> unrecognizedArgs = new ArrayList<>();
        unrecognizedArgs.add("--experimental-options");
        unrecognizedArgs.add("--engine.Compilation=false");
        while (iterator.hasNext()) {
            unrecognizedArgs.add(iterator.next());
        }
        return unrecognizedArgs;
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

    // ----- The LKQL checker -----

    public static final String checkerSource =
            """
            val analysis_units = specified_units()
            val roots = [unit.root for unit in analysis_units]

            map(roots, (root) => node_checker(root))
            map(analysis_units, (unit) => unit_checker(unit))
            """;
}
