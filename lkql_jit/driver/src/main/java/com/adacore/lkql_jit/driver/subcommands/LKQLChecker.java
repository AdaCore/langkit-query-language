//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.driver.checker.*;
import com.adacore.lkql_jit.driver.diagnostics.SarifReportCreator;
import com.adacore.lkql_jit.driver.diagnostics.TextReportCreator;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLList;
import de.jcup.sarif_2_1_0.SarifSchema210ImportExportSupport;
import de.jcup.sarif_2_1_0.model.SarifSchema210;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.io.IOAccess;
import picocli.CommandLine;

/**
 * This is the LKQL checker entry point, this driver loads and run LKQL rules on a provided code
 * base, then report all rule violations.
 */
@CommandLine.Command(
    name = "check",
    description = "Alternative checker driver. Like GNATcheck but with less options & a more " +
        "modern command line interface"
)
public class LKQLChecker extends BaseSubcommand {

    // ----- Attributes -----

    @CommandLine.Spec
    protected CommandLine.Model.CommandSpec spec;

    @CommandLine.Mixin
    EngineArgs engineArgs;

    @CommandLine.Mixin
    GPRArgs gprArgs;

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
        names = { "--auto-fix-mode" },
        description = "For rules with an available auto-fixing function, how to apply it (default" +
            " is DISABLED)%nPossible values: ${COMPLETION-CANDIDATES}",
        completionCandidates = AutoFixModeCompletion.class
    )
    public CheckerRun.AutoFixMode autoFixMode = CheckerRun.AutoFixMode.DISABLED;

    @CommandLine.Unmatched
    public List<String> unmatched = new ArrayList<>();

    // ----- Constructors -----

    /** Simply initialized arguments. */
    public LKQLChecker() {}

    // ----- Abstract methods -----

    /**
     * Perform a custom post-processing on rule instances that are going to be executed. By default,
     * this method check instances validity and filter out invalid ones.
     */
    private List<RuleInstance> postProcessInstances(List<RuleInstance> ruleInstances) {
        return ruleInstances
            .stream()
            .filter(i -> i.isValid(diagnostics))
            .toList();
    }

    // ----- Instance methods -----

    @Override
    public Integer call() throws Exception {
        launch(unmatched.toArray(new String[0]));
        return 0;
    }

    /** The help message comes from the defined arguments. */
    @Override
    protected void printHelp(@SuppressWarnings("unused") OptionCategory maxCategory) {
        spec.commandLine().usage(spec.commandLine().getOut());
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
        return unmatched != null ? unmatched : List.of();
    }

    /** Perform the checking logic and check its exit code. */
    @Override
    protected void launch(Context.Builder contextBuilder) {
        // Create the option object for the context builder
        var optionsBuilder = new LKQLOptions.Builder()
            .engineMode(LKQLOptions.EngineMode.INTERPRETER)
            .files(files)
            .ignores(ignores);
        engineArgs.fillEngineOptions(optionsBuilder);
        gprArgs.fillGPROptions(optionsBuilder);

        // Configure the execution context
        contextBuilder
            .allowIO(IOAccess.ALL)
            .useSystemExit(true)
            .logHandler(logHandler)
            .option("lkql.options", optionsBuilder.build().toJson().toString());

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
                debug,
                ruleInstances,
                context,
                analysisContext,
                specifiedUnits,
                autoFixMode
            );
            checkerRun.start(diagnostics);

            // Display all diagnostics in the required format
            switch (reportFormat) {
                case TEXT -> diagnostics.createReport(
                    new TextReportCreator(System.out, supportAnsi)
                );
                case SARIF -> {
                    var sarifReport = new SarifSchema210();
                    var sarifReportCreator = new SarifReportCreator(
                        sarifReport,
                        spec.parent().version()[0],
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
        for (var rulesDir : rulesDirs) {
            res.add(Paths.get(rulesDir));
        }

        // Then look in the "LKQL_PATH" environment variable
        res.addAll(lkqlPaths());

        return res;
    }

    /** Get all rule instances to run for the current run. */
    private List<RuleInstance> getRuleInstances(Context context, RuleRepository repository) {
        var res = new ArrayList<>(processCommandLineInstances(context, repository));
        if (ruleFile != null) res.addAll(
            Utils.processLKQLRuleFile(diagnostics, context, repository, ruleFile)
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
        for (var arg : rulesArgs) {
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
        for (String ruleName : rules) {
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

    // ----- Inner classes -----

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

    /** Utility class to provide auto-complete for the auto-fix mode. */
    public static class AutoFixModeCompletion implements Iterable<String> {

        @Override
        public Iterator<String> iterator() {
            return Arrays.stream(CheckerRun.AutoFixMode.values()).map(Object::toString).iterator();
        }
    }
}
