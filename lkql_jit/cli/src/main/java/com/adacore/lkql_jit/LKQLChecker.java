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
 * This class represents the LKQL checker entry point with the LKQL JIT backend. This is a TEMPORARY
 * driver to perform efficiency tests on LKQL JIT in real life use case. TODO : Support all flags
 * and options of the lkql_checker original implementation.
 *
 * @author Hugo GUERRIER
 */
public class LKQLChecker extends AbstractLanguageLauncher {

    // ----- Macros and enums -----

    @CommandLine.Command(
            name = "check",
            description =
                    "Alternative checker driver. Like GNATcheck but with less options "
                            + "& a more modern command line interface")
    public static class Args implements Callable<Integer> {

        @CommandLine.Spec public picocli.CommandLine.Model.CommandSpec spec;

        @CommandLine.Parameters(description = "Files to analyze")
        public List<String> files = new ArrayList<>();

        @CommandLine.Option(
                names = {"-C", "--charset"},
                description = "Charset to use for the source decoding")
        public String charset = null;

        @CommandLine.Option(names = "--RTS", description = "Runtime to pass to GPR")
        public String RTS = null;

        @CommandLine.Option(names = "--target", description = "Hardware target to pass to GPR")
        public String target = null;

        @CommandLine.Option(
                names = {"-P", "--project"},
                description = "Project file to use")
        public String project = null;

        @CommandLine.Option(
                names = {"-U", "--recursive"},
                description =
                        "Process all units in the project tree, excluding externally built"
                                + " projects")
        public boolean recursive;

        @CommandLine.Option(
                names = {"-j", "--jobs"},
                description = "Numbers of jobs to use. If zero, one job per CPU")
        public int jobs = 1;

        @CommandLine.Option(
                names = {"-v", "--verbose"},
                description = "Enable the verbose mode")
        public boolean verbose;

        @CommandLine.Option(
                names = "--rules-dir",
                description = "Additional directories where rules will be sought")
        public List<String> rulesDirs = new ArrayList<>();

        @CommandLine.Option(
                names = {"-r", "--rule"},
                description =
                        "Rule to run on the provided code base (Run all rules if none is"
                                + " provided)")
        public List<String> rules = new ArrayList<>();

        @CommandLine.Option(
                names = {"-a", "--rule-arg"},
                description =
                        "Argument to pass to a rule, with the syntax"
                                + " <rule_name>.<arg_name>=<arg_value>")
        public List<String> rulesArgs = new ArrayList<>();

        enum PropertyErrorRecoveryMode {
            continueAndWarn,
            continueAndLog,
            raiseError
        }

        @CommandLine.Option(names = "--property-error-recovery")
        PropertyErrorRecoveryMode propertyErrorRecovery = PropertyErrorRecoveryMode.continueAndWarn;

        @CommandLine.Option(
                names = {"-I", "--ignores"},
                description = "Ada files to ignore during analysis")
        public List<String> ignores = new ArrayList<>();

        @CommandLine.Option(
                names = "--keep-going-on-missing-file",
                description = "Keep going on missing file")
        public Boolean keepGoingOnMissingFile = false;

        @CommandLine.Unmatched public List<String> unmatched;

        @Override
        public Integer call() {
            String[] unmatchedArgs;
            if (this.unmatched == null) {
                unmatchedArgs = new String[0];
            } else {
                unmatchedArgs = this.unmatched.toArray(new String[0]);
            }
            new LKQLChecker(this).launch(unmatchedArgs);
            return 0;
        }
    }

    public LKQLChecker(LKQLChecker.Args args) {
        this.args = args;
    }

    private LKQLChecker.Args args = null;

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
        this.args.spec.commandLine().usage(this.args.spec.commandLine().getOut());
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
     * Start the LQKL checker.
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
     * Execute the LKQL checker script and return the exit code.
     *
     * @param contextBuilder The context builder.
     * @return The exit code of the script.
     */
    protected int executeScript(Context.Builder contextBuilder) {
        // Create the LKQL options object builder
        final var optionsBuilder = new LKQLOptions.Builder();

        // Set the common configurations
        contextBuilder
                .allowIO(true)
                // This is needed to make sure that calls to `exitContext` done from within an
                // isolate thread (e.g. executing a Java callback from Ada code) directly stop
                // the program instead of going it the normal way by raising a special exception,
                // as such exceptions won't be handled by the caller when thrown from inside the
                // isolate thread.
                .useSystemExit(true);
        optionsBuilder.checkerDebug(true);

        // Forward the command line options to the options object builder
        optionsBuilder
                .verbose(this.args.verbose)
                .keepGoingOnMissingFile(this.args.keepGoingOnMissingFile)
                .projectFile(this.args.project)
                .files(this.args.files)
                .ignores(this.args.ignores)
                .charset(this.args.charset)
                .target(this.args.target)
                .runtime(this.args.RTS)
                .rulesDir(this.args.rulesDirs)
                .ruleInstances(this.getRuleInstances());

        // Finally, pass the options to the LKQL engine
        contextBuilder.option("lkql.options", optionsBuilder.build().toJson().toString());

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            final Source source = Source.newBuilder("lkql", checkerSource, "checker.lkql").build();
            final Value executable = context.parse(source);
            executable.executeVoid(true);
            return 0;
        } catch (Exception e) {
            if (e instanceof PolyglotException pe && pe.isExit()) {
                return pe.getExitStatus();
            } else if (this.args.verbose) {
                e.printStackTrace();
            } else {
                System.err.println(e.getMessage());
            }
            return 0;
        } catch (Error e) {
            System.err.println(e.getMessage());
            return 1;
        }
    }

    @Override
    protected List<String> preprocessArguments(
            List<String> arguments, Map<String, String> polyglotOptions) {
        if (this.args.unmatched != null) {
            return this.args.unmatched;
        } else {
            return new ArrayList<>();
        }
    }

    // ----- Option parsing helpers -----

    /** Get the rule instances defined be the user through the LKQL checker command-line. */
    private Map<String, RuleInstance> getRuleInstances() {
        // === First, parse the rule arguments in a map
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

        // === Then, parse the provided instances, filling them with the previously parsed arguments
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

    // ----- The LKQL checker -----

    public static final String checkerSource =
            """
            val analysis_units = specified_units()
            val roots = [unit.root for unit in analysis_units]

            map(roots, (root) => node_checker(root))
            map(analysis_units, (unit) => unit_checker(unit))
            """;
}
