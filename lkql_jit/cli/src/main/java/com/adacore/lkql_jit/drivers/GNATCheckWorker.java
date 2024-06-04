//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.drivers;

import com.adacore.lkql_jit.options.JsonUtils;
import com.adacore.lkql_jit.options.RuleInstance;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;
import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
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
                names = {"--parse-lkql-config"},
                description =
                        "Parse the given LKQL file as a rule configuration file and return its"
                            + " result as a JSON encoded string. If this option is provided, all"
                            + " other features are disabled.")
        public String lkqlConfigFile = null;

        @CommandLine.Option(
                names = {"-C", "--charset"},
                description = "Charset to use for the source decoding")
        public String charset = null;

        @CommandLine.Option(names = "--RTS", description = "Runtime to pass to GPR")
        public String RTS = null;

        @CommandLine.Option(names = "--target", description = "Target to pass to GPR")
        public String target = null;

        @CommandLine.Option(names = "--config", description = "Config file for GPR loading")
        public String configFile = null;

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

        @CommandLine.Option(
                names = "--rules-dir",
                description = "Additional directory in which to check for rules")
        public List<String> rulesDirs = new ArrayList<>();

        @CommandLine.Option(names = "--rules-from", description = "The file containing the rules")
        public List<String> rulesFroms = null;

        @CommandLine.Option(names = "--files-from", description = "The file containing the files")
        public String filesFrom = null;

        @CommandLine.Option(names = "--out-file", description = "The output report file")
        public String outputFile = null;

        @CommandLine.Option(
                names = "--ignore-project-switches",
                description =
                        "Process all units in the project tree, excluding externally built"
                                + " projects")
        public boolean ignoreProjectSwitches;

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

        // If a LKQL rule config file has been provided, parse it and display the result
        if (this.args.lkqlConfigFile != null) {
            System.out.println(
                    JsonUtils.serializeInstances(parseLKQLRuleFile(this.args.lkqlConfigFile)));
            return 0;
        }

        // Set the context options
        if (this.args.verbose) {
            contextBuilder.option("lkql.verbose", "true");
        }

        // Set the project file
        if (this.args.project != null) {
            contextBuilder.option("lkql.projectFile", this.args.project);
        }

        if (this.args.subProject != null) {
            contextBuilder.option("lkql.subprojectFile", this.args.subProject);
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

        if (this.args.target != null) {
            contextBuilder.option("lkql.target", this.args.target);
        }

        if (this.args.RTS != null) {
            contextBuilder.option("lkql.runtime", this.args.RTS);
        }

        if (this.args.configFile != null) {
            contextBuilder.option("lkql.configFile", this.args.configFile);
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
        if (this.args.charset != null) {
            contextBuilder.option("lkql.charset", this.args.charset);
        }

        // Set the rule directories
        if (!this.args.rulesDirs.isEmpty()) {
            contextBuilder.option(
                    "lkql.rulesDirs", String.join(File.pathSeparator, this.args.rulesDirs));
        }

        // Set the rule instances
        final Map<String, RuleInstance> instances = new HashMap<>();
        for (var rulesFrom : this.args.rulesFroms) {
            if (!rulesFrom.isEmpty()) {
                instances.putAll(parseLKQLRuleFile(rulesFrom));
            }
        }
        contextBuilder.option("lkql.ruleInstances", JsonUtils.serializeInstances(instances));

        contextBuilder.engine(
                Engine.newBuilder()
                        .allowExperimentalOptions(true)
                        .option("engine.Compilation", "false")
                        .build());

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

    protected List<String> preprocessArguments(
            List<String> arguments, Map<String, String> polyglotOptions) {
        if (this.args.unmatched != null) {
            return this.args.unmatched;
        } else {
            return new ArrayList<>();
        }
    }

    // ----- Option parsing helpers -----

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
                        if (argName.equals("instance_name")) {
                            final String aliasName =
                                    argObject.getMember("instance_name").asString();
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
