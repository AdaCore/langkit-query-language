//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import java.util.*;
import java.util.concurrent.Callable;
import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import picocli.CommandLine;

/**
 * This class defines the base for all checker-like LKQL entry points. It means that all subcommands
 * that need to be aware of a sources fetching method and a set of rules to run should extend this
 * class.
 */
public abstract class BaseLKQLChecker extends AbstractLanguageLauncher {

    // ----- Properties -----

    /** Parsed arguments from the command-line. */
    protected Args args;

    /** Common source to perform checker like process. */
    public static final String checkerSource =
            """
        val analysis_units = specified_units()
        val roots = [unit.root for unit in analysis_units]

        map(roots, (root) => node_checker(root))
        map(analysis_units, (unit) => unit_checker(unit))
        """;

    // ----- Constructors -----

    /** Simply initialized arguments. */
    protected BaseLKQLChecker(Args args) {
        this.args = args;
    }

    // ----- Abstract methods -----

    /** Get options to run the checker source with. */
    protected abstract LKQLOptions getOptions();

    // ----- Instance methods -----

    /** The help message comes from the defined arguments. */
    @Override
    protected void printHelp(@SuppressWarnings("unused") OptionCategory maxCategory) {
        args.spec.commandLine().usage(args.spec.commandLine().getOut());
    }

    /** Language ID is always "lkql". */
    @Override
    protected String getLanguageId() {
        return "lkql";
    }

    /** Arguments passed to this method are always JVM/GraalVM specific. */
    @Override
    protected List<String> preprocessArguments(
            List<String> arguments,
            @SuppressWarnings("unused") Map<String, String> polyglotOptions) {
        return arguments;
    }

    /** Perform the checking logic and check its exit code. */
    @Override
    protected void launch(Context.Builder contextBuilder) {
        // Configure the execution context
        contextBuilder
                .allowIO(true)
                // This is needed to make sure that calls to `exitContext` done from within an
                // isolate thread (e.g. executing a Java callback from Ada code) directly stop
                // the program instead of going it the normal way by raising a special exception,
                // as such exceptions won't be handled by the caller when thrown from inside the
                // isolate thread.
                .useSystemExit(true);

        // Get options defined by the subcommand and pass them to the engine
        final var options = getOptions();
        contextBuilder.option("lkql.options", options.toJson().toString());

        // Create the context and run the checker source in it
        int exitCode;
        try (Context context = contextBuilder.build()) {
            final Source source = Source.newBuilder("lkql", checkerSource, "checker.lkql").build();
            final Value executable = context.parse(source);
            executable.executeVoid(true);
            exitCode = 0;
        } catch (Exception e) {
            if (e instanceof PolyglotException pe && pe.isExit()) {
                exitCode = pe.getExitStatus();
            } else {
                if (this.args.verbose) {
                    e.printStackTrace();
                } else {
                    System.err.println(e.getMessage());
                }
                exitCode = 0;
            }
        } catch (Error e) {
            System.err.println(e.getMessage());
            exitCode = 1;
        }

        // According to the exit code, forward it to the caller
        if (exitCode != 0) {
            this.abort((String) null, exitCode);
        }
    }

    // ----- Options creation helpers -----

    /** Get an LKQL options builder pre-filled with the known options. */
    protected LKQLOptions.Builder getBaseOptionsBuilder() {
        return new LKQLOptions.Builder()
                .verbose(this.args.verbose)
                .files(this.args.files)
                .ignores(this.args.ignores)
                .charset(this.args.charset)
                .runtime(this.args.rts)
                .target(this.args.target)
                .projectFile(this.args.project)
                .rulesDir(this.args.rulesDirs)
                .ruleInstances(getRuleInstances())
                .keepGoingOnMissingFile(this.args.keepGoingOnMissingFile);
    }

    /** Get the rule instances defined be the user through the LKQL checker command-line. */
    protected Map<String, RuleInstance> getRuleInstances() {
        // First, parse the rule arguments in a map
        Map<String, Map<String, String>> instanceArgs = new HashMap<>();
        for (String arg : this.args.rulesArgs) {
            // Verify that the rule argument is not empty
            if (arg.isEmpty() || arg.isBlank()) continue;

            // Split the get the names and the value
            final String[] valueSplit = arg.split("=");
            final String[] nameSplit = valueSplit[0].split("\\.");

            // Verify the rule argument syntax
            if (valueSplit.length != 2 || nameSplit.length != 2) {
                System.err.println("Rule argument syntax error: '" + arg + "'");
                continue;
            }

            // Get the information from the rule argument source
            final String instanceId = nameSplit[0].toLowerCase().trim();
            final String argName = nameSplit[1].toLowerCase().trim();
            final String argValue = valueSplit[1].trim();

            Map<String, String> ruleArgs = instanceArgs.getOrDefault(instanceId, new HashMap<>());
            ruleArgs.put(argName, argValue);
            instanceArgs.put(instanceId, ruleArgs);
        }

        // Then, parse the provided instances, filling them with the previously parsed arguments
        HashMap<String, RuleInstance> res = new HashMap<>();
        for (String ruleName : this.args.rules) {
            final String instanceId = ruleName.toLowerCase();
            res.put(
                    instanceId,
                    new RuleInstance(
                            ruleName,
                            Optional.empty(),
                            RuleInstance.SourceMode.GENERAL,
                            instanceArgs.get(instanceId)));
        }
        return res;
    }

    // ----- Inner classes -----

    /** This class defines all common CLI arguments for checker-like subcommands. */
    public abstract static class Args implements Callable<Integer> {

        @CommandLine.Spec public picocli.CommandLine.Model.CommandSpec spec;

        @CommandLine.Option(
                names = {"-v", "--verbose"},
                description = "Enable the verbose mode")
        public boolean verbose;

        @CommandLine.Parameters(description = "Files to analyze")
        public List<String> files = new ArrayList<>();

        @CommandLine.Option(
                names = {"-I", "--ignores"},
                description = "Files to ignore during analysis")
        public List<String> ignores = new ArrayList<>();

        @CommandLine.Option(
                names = {"-C", "--charset"},
                description = "Charset to use for the source decoding")
        public String charset = null;

        @CommandLine.Option(names = "--RTS", description = "Runtime to pass to GPR")
        public String rts = null;

        @CommandLine.Option(names = "--target", description = "Hardware target to pass to GPR")
        public String target = null;

        @CommandLine.Option(
                names = {"-P", "--project"},
                description = "Project file to use")
        public String project = null;

        @CommandLine.Option(
                names = "--rules-dir",
                description = "Additional directories where rules will be sought")
        public List<String> rulesDirs = new ArrayList<>();

        @CommandLine.Option(
                names = {"-r", "--rule"},
                description =
                        "Rules to run on the provided code base (run all rules if none is "
                                + "provided)")
        public List<String> rules = new ArrayList<>();

        @CommandLine.Option(
                names = {"-a", "--rule-arg"},
                description =
                        "Argument to pass to a rule, with the syntax"
                                + " <rule_name>.<arg_name>=<arg_value>")
        public List<String> rulesArgs = new ArrayList<>();

        @CommandLine.Option(
                names = "--keep-going-on-missing-file",
                description = "Keep going on missing file")
        public Boolean keepGoingOnMissingFile = false;

        @CommandLine.Unmatched public List<String> unmatched = new ArrayList<>();
    }
}
