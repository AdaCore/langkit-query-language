//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.driver.checker.CheckerRun;
import com.adacore.lkql_jit.driver.checker.RuleInstance;
import com.adacore.lkql_jit.driver.checker.RuleRepository;
import com.adacore.lkql_jit.driver.diagnostics.TextReportCreator;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLList;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.io.IOAccess;
import picocli.CommandLine;

/**
 * This class defines the base for all checker-like LKQL entry points. It means that all subcommands
 * that need to be aware of a sources fetching method and a set of rules to run should extend this
 * class.
 */
public abstract class BaseLKQLChecker extends BaseSubcommand {

    // ----- Attributes -----

    /** Parsed arguments from the command-line. */
    protected Args args;

    // ----- Constructors -----

    /** Simply initialized arguments. */
    protected BaseLKQLChecker(Args args) {
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

            // Display all diagnostics
            diagnostics.createReport(new TextReportCreator(System.out, supportAnsi));
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
        }

        // Then, parse the provided instances, filling them with the previously parsed arguments
        List<RuleInstance> res = new ArrayList<>();
        for (String ruleName : this.args.rules) {
            var ruleNameLower = ruleName.toLowerCase();
            var instantiatedRule = repository.getRuleByName(ruleNameLower);
            if (instantiatedRule.isPresent()) {
                res.add(
                    new RuleInstance(
                        context,
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

    // ----- Inner classes -----

    /** This class defines all common CLI arguments for checker-like subcommands. */
    public abstract static class Args implements Callable<Integer> {

        @CommandLine.Spec
        public picocli.CommandLine.Model.CommandSpec spec;

        @CommandLine.Option(names = { "-v", "--verbose" }, description = "Enable the verbose mode")
        public boolean verbose;

        @CommandLine.Option(names = { "-d", "--debug" }, description = "Enable the debug mode")
        public boolean debug;

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
            names = "--missing-file-is-error",
            description = "Consider and log missing files as errors"
        )
        public Boolean missingFileIsError = false;

        @CommandLine.Unmatched
        public List<String> unmatched = new ArrayList<>();
    }
}
