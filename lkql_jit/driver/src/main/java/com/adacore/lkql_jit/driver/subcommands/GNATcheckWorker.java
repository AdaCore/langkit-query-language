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
import com.adacore.lkql_jit.driver.checker.Utils;
import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.SarifReportCreator;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLList;
import de.jcup.sarif_2_1_0.SarifSchema210ImportExportSupport;
import de.jcup.sarif_2_1_0.model.SarifSchema210;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.*;
import org.graalvm.polyglot.io.IOAccess;
import picocli.CommandLine;

/** Implement a worker process for the GNATcheck driver. */
@CommandLine.Command(
    name = "gnatcheck_worker",
    description = "Internal driver meant to be called by GNATcheck. Not for public use!"
)
public class GNATcheckWorker extends BaseSubcommand {

    // ----- Attributes -----

    @CommandLine.Spec
    protected CommandLine.Model.CommandSpec spec;

    @CommandLine.Mixin
    EngineArgs engineArgs;

    @CommandLine.Mixin
    GPRArgs gprArgs;

    @CommandLine.Option(names = { "-v", "--verbose" }, description = "Enable the verbose mode")
    public boolean verbose;

    @CommandLine.Option(names = "-d", description = "Enable the debug mode")
    public boolean debug;

    @CommandLine.Option(
        names = { "--parse-lkql-config" },
        description = "Parse the given LKQL file as a rule configuration file and return its" +
            " result as a SARIF report. If this option is provided, all other features are disabled"
    )
    public Path lkqlConfigToProcess;

    @CommandLine.Option(
        names = "-A",
        description = "The name of the subproject to analyse, if any. This implies that" +
            " `projectFile` designates an aggregate project."
    )
    public String subProject;

    @CommandLine.Option(
        names = "--rules-dir",
        description = "Additional directory in which to check for rules"
    )
    public List<Path> rulesDirs = new ArrayList<>();

    @CommandLine.Option(
        names = "--rules-from",
        description = "The file containing all rule instances to run in the LKQL format"
    )
    public Path rulesFrom;

    @CommandLine.Option(
        names = "--files-from",
        description = "The file containing the list of files to analyze"
    )
    public Path filesFrom;

    @CommandLine.Option(
        names = "--report-instantiation-chain",
        description = "Include the instantiation chain in the report"
    )
    public boolean reportInstantiationChain;

    @CommandLine.Option(names = "--emit-fixes", description = "Include auto fixes in the report")
    public boolean exportAutoFix;

    @CommandLine.Unmatched
    public List<String> unmatched = new ArrayList<>();

    /** A set of rule names that accept the "sole arg" notation. */
    private static final Set<String> ACCEPT_SOLE_ARGS = Set.of("style_checks", "warnings");

    // ----- Constructors -----

    public GNATcheckWorker() {}

    // ----- Instance methods -----

    @Override
    public Integer call() {
        launch(unmatched.toArray(new String[0]));
        return 0;
    }

    /** Display the help message for the LKQL language. */
    @Override
    protected void printHelp(OptionCategory maxCategory) {
        System.out.println("No help!");
    }

    /** Simply return the language id. */
    @Override
    protected String getLanguageId() {
        return Constants.LKQL_ID;
    }

    protected List<String> preprocessArguments(
        List<String> arguments,
        Map<String, String> polyglotOptions
    ) {
        return unmatched;
    }

    /** Run the GNATcheck worker. */
    @Override
    protected void launch(Context.Builder contextBuilder) {
        // Create the LKQL options object builder
        final var optionsBuilder = new LKQLOptions.Builder()
            .additionalLkqlPaths(rulesDirs.stream().map(Path::toString).toList())
            .subprojectFile(subProject)
            .missingFileIsError(false);
        engineArgs.fillEngineOptions(optionsBuilder);
        gprArgs.fillGPROptions(optionsBuilder);

        // We don't show project diagnostics in GNATcheck mode because the driver already handles
        // them.
        optionsBuilder.hideProjectDiagnostics(true);

        // Forward files to analyze to the options builder
        if (filesFrom != null) {
            try {
                optionsBuilder.files(Files.readAllLines(filesFrom));
            } catch (IOException e) {
                diagnostics.add(new Error("Cannot read file: " + filesFrom));
            }
        }

        // Configure the Polyglot context builder
        contextBuilder.logHandler(logHandler);
        contextBuilder.out(System.err);
        contextBuilder.err(System.err);
        contextBuilder.allowIO(IOAccess.ALL);
        contextBuilder.engine(
            Engine.newBuilder()
                .allowExperimentalOptions(true)
                .option("engine.Compilation", "false")
                .build()
        );
        contextBuilder.option("lkql.options", optionsBuilder.build().toJson().toString());

        // Create the list that will contain all rule instances
        var ruleInstances = new ArrayList<RuleInstance>();

        // Create the Polyglot execution context
        try (var context = contextBuilder.build()) {
            // Create a diagnostic collector for rule instances processing
            var ruleInstanceDiagnostics = new DiagnosticCollector();

            // Create and initialize the rule repository
            var searchingDirs = new ArrayList<Path>();
            searchingDirs.addAll(rulesDirs);
            searchingDirs.addAll(lkqlPaths());
            var ruleRepository = new RuleRepository(context, searchingDirs, diagnostics);

            // If there are no errors by now, process instances
            if (!diagnostics.hasError()) {
                // Create a function to process sole args instances
                Utils.RuleArgProcessor soleArgProcessor = ((
                        instantiatedRule,
                        sourceMode,
                        location,
                        argument
                    ) -> {
                        if (
                            ACCEPT_SOLE_ARGS.contains(instantiatedRule.name()) &&
                            argument instanceof String
                        ) {
                            var newInstance = new com.adacore.lkql_jit.driver.checker.RuleInstance(
                                instantiatedRule,
                                Optional.empty(),
                                sourceMode,
                                Map.of("arg", argument),
                                location
                            );
                            return Optional.of(newInstance);
                        }
                        return Optional.empty();
                    });

                // Then add all specified instances to the dedicated list
                ruleInstances.addAll(
                    Utils.postProcessInstances(
                        ruleInstanceDiagnostics,
                        Utils.processLKQLRuleFile(
                            ruleInstanceDiagnostics,
                            context,
                            ruleRepository,
                            lkqlConfigToProcess == null ? rulesFrom : lkqlConfigToProcess,
                            soleArgProcessor
                        ),
                        verbose
                    )
                );
            }

            // If the processing of an LKQL rule file has been requested, the worker shouldn't run
            // the checking process.
            if (
                lkqlConfigToProcess == null &&
                !diagnostics.hasError() &&
                !ruleInstanceDiagnostics.hasError()
            ) {
                // Get analysis context and specified unit from the LKQL engine
                LKQLBaseNamespace namespace = context
                    .eval(Constants.LKQL_ID, "val unts = specified_units()\nval ctx = context()")
                    .as(LKQLBaseNamespace.class);
                var analysisUnits = Arrays.stream(
                    ((LKQLList) namespace.getUncached("unts")).getContent()
                )
                    .map(o -> (LangkitSupport.AnalysisUnit) o)
                    .toList();
                var analysisContext =
                    (LangkitSupport.AnalysisContextInterface) namespace.getUncached("ctx");

                // Then create the checker and run it
                var checker = new CheckerRun(
                    debug,
                    ruleInstances,
                    context,
                    analysisContext,
                    analysisUnits,
                    exportAutoFix
                        ? CheckerRun.AutoFixMode.IN_REPORT
                        : CheckerRun.AutoFixMode.DISABLED,
                    reportInstantiationChain
                );
                checker.start(diagnostics);
            } else {
                diagnostics.addAll(ruleInstanceDiagnostics);
            }
        } finally {
            // In any case, create and emit the SARIF report
            var sarifReport = new SarifSchema210();
            var sarifReportCreator = new SarifReportCreator(
                sarifReport,
                spec.parent().version()[0],
                ruleInstances,
                !diagnostics.hasError()
            );
            diagnostics.createReport(sarifReportCreator);

            // Display the SARIF report on the standard output
            var sarifExporter = new SarifSchema210ImportExportSupport();
            try {
                System.out.println(sarifExporter.toJSON(sarifReport));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
