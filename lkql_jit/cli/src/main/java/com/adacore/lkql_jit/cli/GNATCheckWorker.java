//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.options.RuleInstance;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;
import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.json.JSONObject;
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
                names = "--log-file",
                description = "The file used by the worker to output logs")
        public String gnatcheckLogFile = null;

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
        // Create the LKQL options object builder
        final var optionsBuilder = new LKQLOptions.Builder();

        // Set the common configuration
        contextBuilder.allowIO(true);
        contextBuilder.engine(
                Engine.newBuilder()
                        .allowExperimentalOptions(true)
                        .option("engine.Compilation", "false")
                        .build());
        optionsBuilder
                .diagnosticOutputMode(LKQLOptions.DiagnosticOutputMode.GNATCHECK)
                .fallbackToAllRules(false)
                .keepGoingOnMissingFile(true);

        // If a LKQL rule config file has been provided, parse it and display the result
        if (this.args.lkqlConfigFile != null) {
            try {
                final var instances = parseLKQLRuleFile(this.args.lkqlConfigFile);
                final var jsonInstances =
                        new JSONObject(
                                instances.entrySet().stream()
                                        .map(e -> Map.entry(e.getKey(), e.getValue().toJson()))
                                        .collect(
                                                Collectors.toMap(
                                                        Map.Entry::getKey, Map.Entry::getValue)));
                System.out.println(jsonInstances);
            } catch (LKQLRuleFileError e) {
                System.out.println(e.getMessage());
            }
            return 0;
        }

        // Forward the command line options to the options object builder
        optionsBuilder
                .engineMode(LKQLOptions.EngineMode.CHECKER)
                .verbose(this.args.verbose)
                .projectFile(this.args.project)
                .subprojectFile(this.args.subProject)
                .scenarioVariables(this.args.scenarioVariables)
                .target(this.args.target)
                .runtime(this.args.RTS)
                .configFile(this.args.configFile)
                .charset(this.args.charset)
                .rulesDir(this.args.rulesDirs)
                .showInstantiationChain(this.args.showInstantiationChain)
                .checkerDebug(this.args.debug);

        // Read the list of sources to analyze provided by GNATcheck driver
        if (this.args.filesFrom != null) {
            try {
                optionsBuilder.files(Files.readAllLines(Paths.get(this.args.filesFrom)));
            } catch (IOException e) {
                System.err.println("WORKER_ERROR: Could not read file: " + this.args.filesFrom);
            }
        }

        // Parse the rule instances provided by the GNATcheck driver
        final Map<String, RuleInstance> instances = new HashMap<>();
        for (var rulesFrom : this.args.rulesFroms) {
            if (!rulesFrom.isEmpty()) {
                try {
                    instances.putAll(parseLKQLRuleFile(rulesFrom));
                } catch (LKQLRuleFileError e) {
                    System.out.println(e.getMessage());
                    return 0;
                }
            }
        }
        optionsBuilder.ruleInstances(instances);

        // Finally, pass the options to the LKQL engine
        contextBuilder.option("lkql.options", optionsBuilder.build().toJson().toString());

        try {
            // Install a log handler only if gnatcheckLogFile is set
            if (this.args.gnatcheckLogFile != null) {
                var logFile = FileSystems.getDefault().getPath(this.args.gnatcheckLogFile);
                OutputStream outputStream = new FileOutputStream(logFile.toFile());
                contextBuilder.logHandler(outputStream);
            }
        } catch (FileNotFoundException e) {
            System.err.println(
                    "WORKER_ERROR: Could not create log file: " + this.args.gnatcheckLogFile);
        }

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            final Source source = Source.newBuilder("lkql", checkerSource, "checker.lkql").build();
            context.eval(source);
            return 0;
        } catch (Exception e) {
            System.out.println(e.getMessage());
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

    /** Throws an exception with the given message, related ot the provided LKQL file name. */
    private static void errorInLKQLRuleFile(final String lkqlRuleFile, final String message)
            throws LKQLRuleFileError {
        errorInLKQLRuleFile(lkqlRuleFile, message, true);
    }

    private static void errorInLKQLRuleFile(
            final String lkqlRuleFile, final String message, final boolean addTag)
            throws LKQLRuleFileError {
        throw new LKQLRuleFileError(
                (addTag ? "WORKER_ERROR: " : "") + message + " (" + lkqlRuleFile + ")");
    }

    /**
     * Read the given LKQL file and parse it as a rule configuration file to return the list of
     * instances defined in it.
     *
     * @throws LKQLRuleFileError If there is any error in the provided LKQL rule file, preventing
     *     the analysis to go further.
     */
    private static Map<String, RuleInstance> parseLKQLRuleFile(final String lkqlRuleFileName)
            throws LKQLRuleFileError {
        final File lkqlFile = new File(lkqlRuleFileName);
        final String lkqlFileBasename = lkqlFile.getName();
        final Map<String, RuleInstance> res = new HashMap<>();
        try (Context context =
                Context.newBuilder()
                        .option(
                                "lkql.options",
                                new LKQLOptions.Builder()
                                        .diagnosticOutputMode(
                                                LKQLOptions.DiagnosticOutputMode.GNATCHECK)
                                        .build()
                                        .toJson()
                                        .toString())
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
        } catch (LKQLRuleFileError e) {
            throw e;
        } catch (Exception e) {
            errorInLKQLRuleFile(lkqlFileBasename, e.getMessage(), false);
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
            final Map<String, RuleInstance> toPopulate)
            throws LKQLRuleFileError {
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
            final Map<String, RuleInstance> toPopulate)
            throws LKQLRuleFileError {
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

    // ----- Inner classes -----

    /** An exception to throw while analysing an LKQL rule file. */
    static final class LKQLRuleFileError extends Exception {
        public LKQLRuleFileError(String message) {
            super(message);
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
