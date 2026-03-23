//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.driver.checker.CheckerRun;
import com.adacore.lkql_jit.driver.checker.Rule;
import com.adacore.lkql_jit.driver.checker.RuleInstance;
import com.adacore.lkql_jit.driver.checker.RuleRepository;
import com.adacore.lkql_jit.driver.diagnostics.SarifReportCreator;
import com.adacore.lkql_jit.driver.diagnostics.TextReportCreator;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLDynamicObject;
import com.adacore.lkql_jit.values.interop.LKQLList;
import de.jcup.sarif_2_1_0.SarifSchema210ImportExportSupport;
import de.jcup.sarif_2_1_0.model.SarifSchema210;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import picocli.CommandLine;

/**
 * This class defines the base for all checker-like LKQL entry points. It means that all subcommands
 * that need to be aware of a sources fetching method and a set of rules to run should extend this
 * class.
 */
public abstract class BaseCheckerSubcommand extends BaseSubcommand {

    // ----- Attributes -----

    /** Parsed arguments from the command-line. */
    protected Args args;

    // ----- Constructors -----

    /** Simply initialized arguments. */
    protected BaseCheckerSubcommand(Args args) {
        this.args = args;
    }

    // ----- Abstract methods -----

    /** Perform a custom post-processing on rule instances that are going to be executed. */
    protected List<RuleInstance> postProcessInstances(List<RuleInstance> ruleInstances) {
        return ruleInstances;
    }

    /** Get the mode to apply auto-fixes in. */
    protected abstract CheckerRun.AutoFixMode getAutoFixMode();

    // ----- Instance methods -----

    /** The help message comes from the defined arguments. */
    @Override
    protected void printHelp(@SuppressWarnings("unused") OptionCategory maxCategory) {
        args.spec.commandLine().usage(args.spec.commandLine().getOut());
    }

    @Override
    protected String getLanguageId() {
        return Constants.LKQL_ID;
    }

    /** Arguments passed to this method are always JVM/GraalVM specific. */
    @Override
    protected List<String> preprocessArguments(
        List<String> arguments,
        @SuppressWarnings("unused") Map<String, String> polyglotOptions
    ) {
        return this.args.unmatched != null ? this.args.unmatched : List.of();
    }

    /** Perform the checking logic and check its exit code. */
    @Override
    protected void launch(Context.Builder contextBuilder) {
        // Create the option object for the context builder
        LKQLOptions options = new LKQLOptions.Builder()
            .engineMode(LKQLOptions.EngineMode.INTERPRETER)
            .verbose(this.args.verbose)
            .files(this.args.files)
            .ignores(this.args.ignores)
            .charset(this.args.charset)
            .runtime(this.args.rts)
            .target(this.args.target)
            .projectFile(this.args.project)
            .missingFileIsError(this.args.missingFileIsError)
            .build();

        // Configure the execution context
        contextBuilder
            .allowIO(IOAccess.ALL)
            .useSystemExit(true)
            .logHandler(logHandler)
            .option("lkql.options", options.toJson().toString());

        // Then build the context and perform the checking process
        try (Context context = contextBuilder.build()) {
            RuleRepository repository = new RuleRepository(context, searchingDirs(), diagnostics);
            List<RuleInstance> ruleInstances = postProcessInstances(
                this.getRuleInstances(context, repository)
            );

            // Get analysis context and specified unit from the LKQL engine
            LKQLBaseNamespace namespace = context
                .eval(Constants.LKQL_ID, "val unts = specified_units()\nval ctx = context()")
                .as(LKQLBaseNamespace.class);
            LKQLList units = (LKQLList) namespace.getUncached("unts");
            LangkitSupport.AnalysisContextInterface analysisContext =
                (LangkitSupport.AnalysisContextInterface) namespace.getUncached("ctx");

            // Create the specified units list
            List<LangkitSupport.AnalysisUnit> specifiedUnits = Arrays.stream(units.getContent())
                .map(o -> (LangkitSupport.AnalysisUnit) o)
                .toList();

            // Create a new checker run with the gathered configuration
            CheckerRun checkerRun = new CheckerRun(
                args.debug,
                linesCache,
                ruleInstances,
                context,
                analysisContext,
                specifiedUnits,
                getAutoFixMode()
            );
            checkerRun.start(diagnostics);

            // Display all diagnostics in the required format
            switch (args.reportFormat) {
                case TEXT -> diagnostics.createReport(
                    new TextReportCreator(System.out, supportAnsi)
                );
                case SARIF -> {
                    var sarifReport = new SarifSchema210();
                    var sarifReportCreator = new SarifReportCreator(
                        sarifReport,
                        args.spec.parent().version()[0],
                        ruleInstances,
                        !diagnostics.hasError()
                    );
                    diagnostics.createReport(sarifReportCreator);

                    var sarifExporter = new SarifSchema210ImportExportSupport();
                    try {
                        System.out.println(sarifExporter.toJSON(sarifReport));
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
    }

    /** Helping function to get the list of directories to look in for LKQL rules. */
    private List<Path> searchingDirs() {
        List<Path> res = new ArrayList<>();

        // Add all CLI provided rules directories
        for (var rulesDir : this.args.rulesDirs) {
            res.add(Paths.get(rulesDir));
        }

        // Then look in the "LKQL_PATH" environment variable
        final var lkqlPath = System.getenv(Constants.LKQL_PATH);
        if (lkqlPath != null) {
            res.addAll(Arrays.stream(lkqlPath.split(File.pathSeparator)).map(Paths::get).toList());
        }

        return res;
    }

    /** Get all rule instances to run for the current run. */
    private List<RuleInstance> getRuleInstances(Context context, RuleRepository repository) {
        var res = new ArrayList<>(processCommandLineInstances(context, repository));
        if (args.ruleFile != null) res.addAll(
            processLkqlRuleFile(context, repository, args.ruleFile)
        );
        return res;
    }

    /** Internal helper to get rule instances defined in through the command-line interface. */
    private List<RuleInstance> processCommandLineInstances(
        Context context,
        RuleRepository repository
    ) {
        // First, parse the rule arguments in a map
        Map<String, Map<String, Object>> instanceArgs = new HashMap<>();
        for (var arg : this.args.rulesArgs) {
            // Verify that the rule argument is not empty
            if (arg.isBlank()) continue;

            // Split the get the names and the value
            var valueSplit = arg.split("=");
            var nameSplit = valueSplit[0].split("\\.");

            // Verify the rule argument syntax
            if (valueSplit.length != 2 || nameSplit.length != 2) {
                diagnostics.add(new Error("Rule argument syntax error: \"" + arg + '"'));
                continue;
            }

            // Get the information from the rule argument source
            var ruleLowerName = nameSplit[0].toLowerCase().trim();
            var argName = nameSplit[1].toLowerCase().trim();
            var argValueSource = valueSplit[1].trim();

            // Evaluate the argument value
            try {
                LKQLBaseNamespace namespace = context
                    .eval(Constants.LKQL_ID, "val arg = " + argValueSource)
                    .as(LKQLBaseNamespace.class);
                Object argValue = namespace.getUncached("arg");

                // Then place the result in the map collection all arguments
                Map<String, Object> ruleArgs = instanceArgs.getOrDefault(
                    ruleLowerName,
                    new HashMap<>()
                );
                ruleArgs.put(argName, argValue);
                instanceArgs.put(ruleLowerName, ruleArgs);
            } catch (PolyglotException e) {
                diagnostics.add(
                    new Error("Invalid rule argument value: \"" + argValueSource + '"')
                );
                diagnostics.handleException(e);
            }
        }

        // Then, parse the provided instances, filling them with the previously parsed arguments
        List<RuleInstance> res = new ArrayList<>();
        for (String ruleName : this.args.rules) {
            var ruleNameLower = ruleName.toLowerCase();
            var instantiatedRule = repository.getRuleByName(ruleNameLower);
            if (instantiatedRule.isPresent()) {
                res.add(
                    new RuleInstance(
                        instantiatedRule.get(),
                        Optional.empty(),
                        RuleInstance.SourceMode.GENERAL,
                        instanceArgs.getOrDefault(ruleNameLower, new HashMap<>()),
                        Optional.empty()
                    )
                );
            } else {
                diagnostics.add(new Error("Unknown rule name \"" + ruleName + '"'));
            }
        }
        return res;
    }

    /**
     * Internal helper used to process an LKQL rule file and extract all rule instances defined
     * in it.
     */
    private List<RuleInstance> processLkqlRuleFile(
        Context context,
        RuleRepository repository,
        Path lkqlRuleFile
    ) {
        try {
            // Evaluate the rule file to get its namespace
            var ruleFileNamespace = context
                .eval(Source.newBuilder("lkql", lkqlRuleFile.toFile()).build())
                .as(LKQLBaseNamespace.class);

            // Prepare working variables and the result
            var generalInstances = ruleFileNamespace.getUncached("rules");
            var adaInstances = ruleFileNamespace.getUncached("ada_rules");
            var sparkInstances = ruleFileNamespace.getUncached("spark_rules");
            var res = new ArrayList<RuleInstance>();

            // Process the general instances object
            if (generalInstances instanceof LKQLDynamicObject obj) {
                res.addAll(
                    processInstancesObject(
                        repository,
                        lkqlRuleFile,
                        obj,
                        RuleInstance.SourceMode.GENERAL
                    )
                );
            } else {
                errorInRuleFile(
                    lkqlRuleFile,
                    "An LKQL rule file must define a \"rules\" top level object"
                );
            }

            // Process the Ada instances object
            if (adaInstances != null) {
                if (adaInstances instanceof LKQLDynamicObject obj) {
                    res.addAll(
                        processInstancesObject(
                            repository,
                            lkqlRuleFile,
                            obj,
                            RuleInstance.SourceMode.ADA
                        )
                    );
                } else {
                    errorInRuleFile(
                        lkqlRuleFile,
                        "Value associated to \"ada_rules\" must be an object"
                    );
                }
            }

            // Process the Spark instances object
            if (sparkInstances != null) {
                if (sparkInstances instanceof LKQLDynamicObject obj) {
                    res.addAll(
                        processInstancesObject(
                            repository,
                            lkqlRuleFile,
                            obj,
                            RuleInstance.SourceMode.SPARK
                        )
                    );
                } else {
                    errorInRuleFile(
                        lkqlRuleFile,
                        "Value associated to \"spark_rules\" must be an object"
                    );
                }
            }

            // Finally return the result
            return res;
        } catch (IOException e) {
            diagnostics.add(
                new Error(
                    "Cannot read the LKQL rule file \"" +
                        lkqlRuleFile.getFileName() +
                        "\" (" +
                        e.getMessage() +
                        ')'
                )
            );
        } catch (PolyglotException e) {
            diagnostics.handleException(e);
        }

        // This is the default return case, an empty instance list
        return List.of();
    }

    /**
     * Process the provided LKQL object as an instance container, and return all instances defined
     * in it. Instances are created with the provided source mode.
     */
    private List<RuleInstance> processInstancesObject(
        RuleRepository repository,
        Path lkqlRuleFile,
        LKQLDynamicObject object,
        RuleInstance.SourceMode sourceMode
    ) {
        // Create the result object
        var res = new ArrayList<RuleInstance>();

        // Process each instantiated rule
        for (var ruleInstancesEntry : object.asMap().entrySet()) {
            // Get the rule identifier
            var ruleId = ruleInstancesEntry.getKey().toLowerCase();
            var instantiatedRule = repository.getRuleByName(ruleId);

            // Start by ensuring the rule exists
            if (instantiatedRule.isEmpty()) {
                errorInRuleFile(
                    lkqlRuleFile,
                    "Unknown rule name \"" + ruleInstancesEntry.getKey() + '"'
                );
                continue;
            }

            // Then process all arguments sets for the rule
            if (ruleInstancesEntry.getValue() instanceof LKQLList argSets) {
                if (argSets.size() == 0) {
                    // If not argument set is provided, create a default instance of the rule
                    res.add(
                        new RuleInstance(
                            instantiatedRule.get(),
                            Optional.empty(),
                            sourceMode,
                            Map.of(),
                            Optional.empty()
                        )
                    );
                } else {
                    for (var maybeArgSet : argSets.getContent()) {
                        if (maybeArgSet instanceof LKQLDynamicObject argSet) {
                            res.add(
                                instantiateWithArgumentSet(
                                    sourceMode,
                                    instantiatedRule.get(),
                                    argSet
                                )
                            );
                        } else {
                            errorInRuleFile(
                                lkqlRuleFile,
                                "Rule arguments must be in an object value"
                            );
                        }
                    }
                }
            } else {
                errorInRuleFile(lkqlRuleFile, "The value associated to a rule name must be a list");
            }
        }

        // Return the result
        return res;
    }

    /** Internal helper to create an instance of the provided rule with an argument set. */
    private RuleInstance instantiateWithArgumentSet(
        RuleInstance.SourceMode sourceMode,
        Rule instantiatedRule,
        LKQLDynamicObject argumentSet
    ) {
        // Process the argument set to extract the new instance config
        var instanceArgs = new HashMap<String, Object>();
        String instanceName = null;
        for (var argEntry : argumentSet.asMap().entrySet()) {
            var argName = argEntry.getKey().toLowerCase();

            // Special case for argument "instance_name" which defines the name of the instance
            if (argName.equals("instance_name")) instanceName = (String) argEntry.getValue();

            // All other arguments are processed normally
            instanceArgs.put(argName, argEntry.getValue());
        }

        // Then return the new instance
        return new RuleInstance(
            instantiatedRule,
            Optional.ofNullable(instanceName),
            sourceMode,
            instanceArgs,
            Optional.empty()
        );
    }

    /** Internal helper to signal an error in an LKQL rule file. */
    private void errorInRuleFile(Path lkqlRuleFile, String message) {
        diagnostics.add(new Error(lkqlRuleFile.getFileName().toString() + ": " + message));
    }

    // ----- Inner classes -----

    /** This class defines all common CLI arguments for checker-like subcommands. */
    public abstract static class Args implements Callable<Integer> {

        @CommandLine.Spec
        public picocli.CommandLine.Model.CommandSpec spec;

        @CommandLine.Option(names = { "-v", "--verbose" }, description = "Enable the verbose mode")
        public boolean verbose;

        @CommandLine.Option(names = { "-d", "--debug" }, description = "Enable the debug mode")
        public boolean debug;

        @CommandLine.Option(
            names = { "-f", "--format" },
            description = "Select the output format (default is TEXT)" +
                "%nPossible values: ${COMPLETION-CANDIDATES}",
            completionCandidates = ReportFormat.Completion.class
        )
        public ReportFormat reportFormat = ReportFormat.TEXT;

        @CommandLine.Parameters(description = "Files to analyze")
        public List<String> files = new ArrayList<>();

        @CommandLine.Option(
            names = { "-I", "--ignores" },
            description = "Files to ignore during analysis"
        )
        public List<String> ignores = new ArrayList<>();

        @CommandLine.Option(
            names = { "-C", "--charset" },
            description = "Charset to use for the source decoding"
        )
        public String charset = null;

        @CommandLine.Option(names = "--RTS", description = "Runtime to pass to GPR")
        public String rts = null;

        @CommandLine.Option(names = "--target", description = "Hardware target to pass to GPR")
        public String target = null;

        @CommandLine.Option(names = { "-P", "--project" }, description = "Project file to use")
        public String project = null;

        @CommandLine.Option(
            names = "--rules-dir",
            description = "Additional directories where rules will be sought"
        )
        public List<String> rulesDirs = new ArrayList<>();

        @CommandLine.Option(
            names = { "-r", "--rule" },
            description = "Rules to run on the provided code base (run all rules if none is " +
                "provided)"
        )
        public List<String> rules = new ArrayList<>();

        @CommandLine.Option(
            names = { "-a", "--rule-arg" },
            description = "Argument to pass to a rule, with the syntax" +
                " <rule_name>.<arg_name>=<arg_value>"
        )
        public List<String> rulesArgs = new ArrayList<>();

        @CommandLine.Option(
            names = { "--rule-file" },
            description = "Provide an LKQL rule file to configure rule instances"
        )
        public Path ruleFile;

        @CommandLine.Option(
            names = "--missing-file-is-error",
            description = "Consider and log missing files as errors"
        )
        public Boolean missingFileIsError = false;

        @CommandLine.Unmatched
        public List<String> unmatched = new ArrayList<>();
    }

    /** Enum used to select the checker output format. */
    public enum ReportFormat {
        TEXT,
        SARIF;

        public static class Completion implements Iterable<String> {

            @Override
            public Iterator<String> iterator() {
                return Arrays.stream(ReportFormat.values()).map(Object::toString).iterator();
            }
        }
    }
}
