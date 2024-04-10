//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import java.io.File;
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
                description = "Additional directories where rules will be sought")
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
        public String ignores = null;

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
        // Set the builder common options
        contextBuilder.allowIO(true);

        contextBuilder.option("lkql.checkerDebug", "true");

        // Set the context options
        if (this.args.verbose) {
            System.out.println("=== LKQL JIT is in verbose mode ===");
            contextBuilder.option("lkql.verbose", "true");
        }

        if (this.args.keepGoingOnMissingFile) {
            contextBuilder.option("lkql.keepGoingOnMissingFile", "true");
        }

        // Set the project file
        if (this.args.project != null) {
            contextBuilder.option("lkql.projectFile", this.args.project);
        }

        // Set the files
        if (!this.args.files.isEmpty()) {
            contextBuilder.option("lkql.files", String.join(File.pathSeparator, this.args.files));
        }

        // Set the charset
        if (this.args.charset != null
                && !this.args.charset.isEmpty()
                && !this.args.charset.isBlank()) {
            contextBuilder.option("lkql.charset", this.args.charset);
        }

        if (this.args.RTS != null) {
            contextBuilder.option("lkql.runtime", this.args.RTS);
        }

        if (this.args.target != null) {
            contextBuilder.option("lkql.target", this.args.target);
        }

        // Set the rule directories
        if (!this.args.rulesDirs.isEmpty()) {
            contextBuilder.option(
                    "lkql.rulesDirs", String.join(File.pathSeparator, this.args.rulesDirs));
        }

        // Set the rule to apply
        if (!this.args.rules.isEmpty()) {
            contextBuilder.option("lkql.rules", String.join(",", this.args.rules).toLowerCase());
        }

        // Set the rule argument
        contextBuilder.option("lkql.rulesArgs", String.join(";", this.args.rulesArgs));

        // Set the Ada files to ignore during the analysis
        if (this.args.ignores != null) {
            contextBuilder.option("lkql.ignores", this.args.ignores);
        }

        // This is needed to make sure that calls to `exitContext` done from within an isolate
        // thread (e.g. executing a Java callback from Ada code) directly stop the program instead
        // of going it the normal way by raising a special exception, as such exceptions won't be
        // handled by the caller when thrown from inside the isolate thread.
        contextBuilder.useSystemExit(true);

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

    // ----- The LKQL checker -----

    public static final String checkerSource =
            """
            val analysis_units = specified_units()
            val roots = [unit.root for unit in analysis_units]

            map(roots, (root) => node_checker(root))
            map(analysis_units, (unit) => unit_checker(unit))
            """;
}
