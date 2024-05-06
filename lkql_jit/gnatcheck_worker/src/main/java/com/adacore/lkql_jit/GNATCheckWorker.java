/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

import com.adacore.lkql_jit.options.JsonUtils;
import com.adacore.lkql_jit.options.RuleInstance;
import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;


/**
 * Implement a worker process for the GNATcheck driver.
 *
 * @author Romain BEGUET
 */
public class GNATCheckWorker extends AbstractLanguageLauncher {

    // ----- Macros and enums -----

    /**
     * The identifier of the LKQL language.
     */
    private static final String ID = "lkql";

    // ----- Launcher options -----

    /**
     * The charset to decode the LKQL sources.
     */
    private String charset = null;

    /**
     * If the project analysis should be recursive.
     */
    private boolean recursive = false;

    /**
     * If the verbose mode should be activated.
     */
    private boolean verbose = false;

    /**
     * The project file to analyse.
     */
    private String projectFile = null;

    /**
     * The name of the subproject to analyse, if any.
     * This implies that `projectFile` designates an aggregate project.
     */
    private String subprojectFile = null;

    /**
     * Whether GNATcheck is in debug mode.
     */
    private boolean debug = false;

    /**
     * The project's scenario variables.
     */
    private String scenarioVars = null;

    /**
     * Whether the '--simple-project' flag was used.
     */
    private boolean isSimpleProject = false;

    /**
     * A directory containing all user added rules.
     */
    private String rulesDirs = null;

    /** The LKQL file to parse as config file. */
    private String lkqlConfigToParse = null;

    /**
     * The rules to apply.
     */
    private String rulesFrom = null;

    /**
     * The LKQL file to configure the rules.
     */
    private String rulesFromLKQL = null;

    /**
     * The file containing the files to analyse.
     */
    private String filesFrom = null;

    /**
     * The source files to ignore during analysis.
     */
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

        // If a LKQL rule config file has been provided, parse it and display the result
        if (this.lkqlConfigToParse != null) {
            System.out.println(
                JsonUtils.serializeInstances(parseLKQLRuleFile(this.lkqlConfigToParse)));
            return 0;
        }

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

        // Set the rule instances
        final Map<String, RuleInstance> instances = new HashMap<>();
        if (this.rulesFrom != null && !this.rulesFrom.isBlank()) {
            instances.putAll(parseGNATcheckRuleFile(this.rulesFrom));
        }
        if (this.rulesFromLKQL != null && !this.rulesFromLKQL.isEmpty()) {
            instances.putAll(parseLKQLRuleFile(this.rulesFromLKQL));
        }
        contextBuilder.option("lkql.ruleInstances", JsonUtils.serializeInstances(instances));

        if (this.ignore != null && !this.ignore.isEmpty() && !this.ignore.isBlank()) {
            contextBuilder.option("lkql.ignores", this.ignore);
        }

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            final Source source = Source.newBuilder("lkql", checkerSource, "checker.lkql")
                .build();
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
     * Get the GNATcheck worker specific argument and return the unparsed ones to the default parser.
     * <p>
     * The expected format for command-line arguments is as follows (the order should be respected):
     * <ul>
     * <li> `--subprocess` No-op here. Originally used by GNATcheck to discriminate between a driver run and
     *      a worker run, since they were implemented by the same application.
     * <li> `[-P{project} [--ignore-project-switches]]` The project file to use, and whether to ignore GNATcheck-
     *      related options that are specified in it.
     * <li> `[--simple-project]`
     * <li> `[-A]` The aggregated project to consider, used when the main project file is an aggregate project.
     * <li> `[--RTS {runtime}]` The runtime to use
     * <li> `[--target {target}]` The target to use
     * <li> `[-d]` Whether to output debug information
     * <li> `[-eL]`
     * <li> `[--no_objects_dir]` Whether to output data in the objects directory. TODO: Not sure if relevant for workers
     * <li> `[--rules-dir {rule_dir}]*` Additional custom directories in which to search for the specified rules
     * <li> `[-U]` Whether to consider project sources recursively. Not relevant for worker, as files to consider are
     *      explicitly given by the driver.
     * <li> `-files {path}` A path to a temporary file generated by the driver, which contains the list of unit
     *      names that this worker should be processing during its run.
     * <li> `[-Xvar=val]*` The GPR scenario variables to use when loading the project file
     * <li> `-rules` No-op.
     * <li> `-from {path}` A path to a temporary file generated by the driver, which contains the list of rule
     *      specifications (to be parsed) that this worker should use during its run
     * <li> `-from-lkql {path}` A path to an LKQL file which contains the list of rule specifications (to be parsed)
     *      that this worker should use during its run
     * </ul>
     *
     * @param arguments       The arguments to parse.
     * @param polyglotOptions The polyglot options.
     * @return The unrecognized options.
     */
    @Override
    protected List<String> preprocessArguments(List<String> arguments, Map<String, String> polyglotOptions) {
        // Iterate over arguments and parse them
        ListIterator<String> iterator = arguments.listIterator();
        String currentArg = iterator.next();

        // Test the first argument value
        if (currentArg.equals("--parse-lkql-config")) {
            this.lkqlConfigToParse = iterator.next();
            return List.of();
        }
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

        // Encode scenario variables specifications (key=value) in Base64 in order to escape `value` which can
        // contain arbitrary characters. Join them with the semicolon character (which cannot appear in a Base64-encoded
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

    // ----- Option parsing helpers -----

    /**
     * Open the given GNATcheck rule file and parse it to extract all user defined rule instances.
     */
    private static Map<String, RuleInstance> parseGNATcheckRuleFile(
        final String gnatcheckRuleFile) {
        String currentInstanceId = null;
        final Map<String, RuleInstance> res = new HashMap<>();
        try {
            for (String ruleSpec :
                Files.readAllLines(Paths.get(gnatcheckRuleFile)).stream()
                    .filter(s -> !s.isBlank())
                    .toList()) {
                if (ruleSpec.startsWith("-")) {
                    final String[] argSplit = ruleSpec.substring(1).split("=");
                    res.get(currentInstanceId).arguments().put(argSplit[0], argSplit[1]);
                } else {
                    final String[] ruleSplit = ruleSpec.split("\\|");
                    final RuleInstance.SourceMode sourceMode =
                        RuleInstance.SourceMode.valueOf(ruleSplit[1]);
                    currentInstanceId = ruleSplit[0].toLowerCase();
                    res.put(
                        currentInstanceId,
                        new RuleInstance(
                            currentInstanceId,
                            Optional.empty(),
                            sourceMode,
                            new HashMap<>()));
                }
            }
        } catch (IOException e) {
            System.err.println("WORKER_FATAL_ERROR: Could not read file: " + gnatcheckRuleFile);
        }
        return res;
    }

    /**
     * Read the given LKQL file and parse it as a rule configuration file to return the extracted
     * instances.
     */
    private static Map<String, RuleInstance> parseLKQLRuleFile(final String lkqlRuleFile) {
        final Map<String, RuleInstance> res = new HashMap<>();
        try (Context context =
                 Context.newBuilder()
                     .option("lkql.diagnosticOutputMode", "GNATCHECK")
                     .allowIO(true)
                     .build()) {
            // Parse the LKQL rule configuration file with a polyglot context
            final Source source = Source.newBuilder("lkql", new File(lkqlRuleFile)).build();
            final Value executable = context.parse(source);
            final Value topLevel = executable.execute(false);

            // Get the mandatory general instances object and populate the result with it
            if (topLevel.hasMember("rules")) {
                processInstancesObject(
                    topLevel.getMember("rules"), RuleInstance.SourceMode.GENERAL, res);

                // Then get the optional Ada and SPARK instances
                if (topLevel.hasMember("ada_rules")) {
                    processInstancesObject(
                        topLevel.getMember("ada_rules"), RuleInstance.SourceMode.ADA, res);
                }
                if (topLevel.hasMember("spark_rules")) {
                    processInstancesObject(
                        topLevel.getMember("spark_rules"), RuleInstance.SourceMode.SPARK, res);
                }
            } else {
                System.err.println(
                    "WORKER_FATAL_ERROR: LKQL config file must define a 'rules' "
                        + "top level object value");
            }
        } catch (IOException e) {
            System.err.println("WORKER_FATAL_ERROR: Could not read file: " + lkqlRuleFile);
        } catch (Exception e) {
            System.err.println(
                "WORKER_FATAL_ERROR: during processing of rule file: "
                    + lkqlRuleFile
                    + ": "
                    + e.getMessage());
        }
        return res;
    }

    /**
     * Internal method to process an instance object, extracted from the LKQL rule config file
     * top-level.
     */
    private static void processInstancesObject(
        final Value instancesObject,
        final RuleInstance.SourceMode sourceMode,
        final Map<String, RuleInstance> toPopulate) {
        // Iterate on all instance object keys
        for (String ruleName : instancesObject.getMemberKeys()) {
            final String lowerRuleName = ruleName.toLowerCase();
            final Value argList = instancesObject.getMember(ruleName);

            // Check that the value associated to the rule name is an array like value
            if (!argList.hasArrayElements()) {
                System.err.println("WORKER_FATAL_ERROR: Rule arguments must be an indexable value");
                continue;
            }

            // If there is no element in the argument list, just create an instance with no argument
            // and no alias.
            final long argListSize = argList.getArraySize();
            if (argListSize == 0) {
                toPopulate.put(
                    lowerRuleName,
                    new RuleInstance(
                        lowerRuleName, Optional.empty(), sourceMode, new HashMap<>()));
            }

            // Else iterate over each argument object and create one instance for each
            else {
                for (long i = 0; i < argListSize; i++) {
                    String instanceId = lowerRuleName;
                    Optional<String> instanceName = Optional.empty();
                    final Map<String, String> arguments = new HashMap<>();
                    final Value argObject = argList.getArrayElement(i);
                    for (String argName : argObject.getMemberKeys()) {
                        if (argName.equals("alias_name")) {
                            final String aliasName = argObject.getMember("alias_name").asString();
                            instanceId = aliasName.toLowerCase();
                            instanceName = Optional.of(aliasName);
                        } else {
                            Value argValue = argObject.getMember(argName);
                            arguments.put(
                                argName,
                                argValue.isString()
                                    ? "\"" + argValue + "\""
                                    : argValue.toString());
                        }
                    }
                    toPopulate.put(
                        instanceId,
                        new RuleInstance(lowerRuleName, instanceName, sourceMode, arguments));
                }
            }
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
