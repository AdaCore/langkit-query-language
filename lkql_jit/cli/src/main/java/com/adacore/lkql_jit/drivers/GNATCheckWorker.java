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

        @CommandLine.Option(
                names = "--ignore-project-switches",
                description =
                        "Process all units in the project tree, excluding externally built"
                                + " projects")
        public boolean ignoreProjectSwitches;

        @CommandLine.Option(
                names = "--show-instantiation-chain",
                description = "Show instantiation chain in reported generic construct")
        public boolean showInstantiationChain;

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

        // Set the generic instantiation displaying parameter
        if (this.args.showInstantiationChain) {
            contextBuilder.option("lkql.showInstantiationChain", "true");
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

    /** Emit a formatted error when there is an invalid LKQL rule file. */
    private static void errorInLKQLRuleFile(final String lkqlRuleFile, final String message) {
        System.err.println("WORKER_FATAL_ERROR: " + message + " (" + lkqlRuleFile + ")");
    }

    /**
     * Read the given LKQL file and parse it as a rule configuration file to return the extracted
     * instances.
     */
    private static Map<String, RuleInstance> parseLKQLRuleFile(final String lkqlRuleFileName) {
        final File lkqlFile = new File(lkqlRuleFileName);
        final String lkqlFileBasename = lkqlFile.getName();
        final Map<String, RuleInstance> res = new HashMap<>();
        try (Context context =
                Context.newBuilder()
                        .option("lkql.diagnosticOutputMode", "GNATCHECK")
                        .allowIO(true)
                        .build()) {
            // Parse the LKQL rule configuration file with a polyglot context
            final Source source = Source.newBuilder("lkql", lkqlFile).build();
            final Value executable = context.parse(source);
            final Value topLevel = executable.execute(false);

            // Get the mandatory general instances object and populate the result with it
            if (topLevel.hasMember("rules")) {
                processInstancesObject(
                        lkqlFileBasename,
                        topLevel.getMember("rules"),
                        RuleInstance.SourceMode.GENERAL,
                        res);

                // Then get the optional Ada and SPARK instances
                if (topLevel.hasMember("ada_rules")) {
                    processInstancesObject(
                            lkqlFileBasename,
                            topLevel.getMember("ada_rules"),
                            RuleInstance.SourceMode.ADA,
                            res);
                }
                if (topLevel.hasMember("spark_rules")) {
                    processInstancesObject(
                            lkqlFileBasename,
                            topLevel.getMember("spark_rules"),
                            RuleInstance.SourceMode.SPARK,
                            res);
                }
            } else {
                errorInLKQLRuleFile(
                        lkqlFileBasename,
                        "LKQL config file must define a 'rules' top level object value");
            }
        } catch (IOException e) {
            errorInLKQLRuleFile(lkqlFileBasename, "Could not read file");
        } catch (Exception e) {
            errorInLKQLRuleFile(
                    lkqlFileBasename, "Error during file processing: " + e.getMessage());
        }
        return res;
    }

    /**
     * Internal method to process an instance object, extracted from the LKQL rule config file
     * top-level.
     */
    private static void processInstancesObject(
            final String lkqlRuleFile,
            final Value instancesObject,
            final RuleInstance.SourceMode sourceMode,
            final Map<String, RuleInstance> toPopulate) {
        // Iterate on all instance object keys
        for (String ruleName : instancesObject.getMemberKeys()) {
            final String lowerRuleName = ruleName.toLowerCase();
            final Value args = instancesObject.getMember(ruleName);

            // Check that the value associated to the rule name is an array like value
            if (args.hasArrayElements()) {
                // If there is no element in the argument list, just create an instance with no
                // argument and no alias.
                if (args.getArraySize() == 0) {
                    if (toPopulate.containsKey(lowerRuleName)) {
                        errorInLKQLRuleFile(
                                lkqlRuleFile,
                                "Multiple instances with the same name: " + lowerRuleName);
                    } else {
                        toPopulate.put(
                                lowerRuleName,
                                new RuleInstance(
                                        lowerRuleName,
                                        Optional.empty(),
                                        sourceMode,
                                        new HashMap<>()));
                    }
                }

                // Else iterate over each argument object and create one instance for each
                else {
                    for (long i = 0; i < args.getArraySize(); i++) {
                        processArgsObject(
                                lkqlRuleFile,
                                args.getArrayElement(i),
                                sourceMode,
                                lowerRuleName,
                                toPopulate);
                    }
                }
            } else if (args.hasMembers()) {
                processArgsObject(lkqlRuleFile, args, sourceMode, lowerRuleName, toPopulate);
            } else {
                // Allow sole arguments for some rules
                if (acceptSoleArgs(lowerRuleName) && args.isString()) {
                    processSoleArg(args, sourceMode, lowerRuleName, toPopulate);
                } else {
                    errorInLKQLRuleFile(
                            lkqlRuleFile, "Rule arguments must be an object or indexable value");
                }
            }
        }
    }

    /** Internal method to process an object value containing arguments for the given rule name. */
    private static void processArgsObject(
            final String lkqlRuleFile,
            final Value argsObject,
            final RuleInstance.SourceMode sourceMode,
            final String ruleName,
            final Map<String, RuleInstance> toPopulate) {
        // Ensure that the given value has members (is an object)
        if (!argsObject.hasMembers()) {
            errorInLKQLRuleFile(lkqlRuleFile, "Arguments should be in an object value");
        }

        // If this is a valid value, process arguments in it
        else {
            // Compute the instance arguments and optional instance name
            String instanceId = ruleName;
            Optional<String> instanceName = Optional.empty();
            Map<String, String> arguments = new HashMap<>();
            for (String argName : argsObject.getMemberKeys()) {
                if (argName.equals("instance_name")) {
                    String aliasName = argsObject.getMember("instance_name").asString();
                    instanceId = aliasName.toLowerCase();
                    instanceName = Optional.of(aliasName);
                } else {
                    Value argValue = argsObject.getMember(argName);
                    arguments.put(
                            argName,
                            argValue.isString() ? "\"" + argValue + "\"" : argValue.toString());
                }
            }

            // Add an instance in the instance map if it is not present
            if (toPopulate.containsKey(instanceId)) {
                errorInLKQLRuleFile(
                        lkqlRuleFile, "Multiple instances with the same name: " + instanceId);
            } else {
                toPopulate.put(
                        instanceId,
                        new RuleInstance(ruleName, instanceName, sourceMode, arguments));
            }
        }
    }

    /** Internal function to process a sole string argument for a compiler-based rule. */
    private static void processSoleArg(
            final Value arg,
            final RuleInstance.SourceMode sourceMode,
            final String ruleName,
            final Map<String, RuleInstance> toPopulate) {
        // Create the new rule instance and add it to the "global" map
        Map<String, String> args = new HashMap<>();
        args.put("arg", "\"" + arg + "\"");
        toPopulate.put(ruleName, new RuleInstance(ruleName, Optional.empty(), sourceMode, args));
    }

    /** Util function which returns whether a rule accepts sole argument. */
    private static boolean acceptSoleArgs(final String ruleName) {
        return List.of("style_checks", "warnings").contains(ruleName);
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
