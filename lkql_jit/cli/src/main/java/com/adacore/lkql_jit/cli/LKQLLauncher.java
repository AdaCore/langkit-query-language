//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import com.adacore.lkql_jit.options.LKQLOptions;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.shadowed.org.jline.reader.EndOfFileException;
import org.graalvm.shadowed.org.jline.reader.LineReader;
import org.graalvm.shadowed.org.jline.reader.LineReaderBuilder;
import org.graalvm.shadowed.org.jline.reader.UserInterruptException;
import picocli.CommandLine;

/**
 * This class is the LKQL launcher, this will handle all execution request coming from the command
 * line.
 *
 * @author Hugo GUERRIER
 *     <p>TODO : Support all features of the original LKQL Ada implementation
 */
public class LKQLLauncher extends AbstractLanguageLauncher {

    @CommandLine.Command(name = "run", description = "Run the LKQL interpreter on a given script")
    public static class LKQLRun implements Callable<Integer> {

        @CommandLine.Spec public CommandLine.Model.CommandSpec spec;

        @CommandLine.Parameters(description = "Files to analyze")
        public List<String> files = new ArrayList<>();

        @CommandLine.Option(
                names = {"-C", "--charset"},
                description = "Charset to use for the source decoding")
        public String charset = null;

        @CommandLine.Option(
                names = {"-P", "--project"},
                description = "Project file to use")
        public String project = null;

        @CommandLine.Option(names = "--RTS", description = "Runtime to pass to GPR")
        public String RTS = null;

        @CommandLine.Option(names = "--target", description = "Hardware target to pass to GPR")
        public String target = null;

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

        @CommandLine.Unmatched public List<String> unmatched;

        @CommandLine.Option(
                names = {"-S", "--script-path"},
                description = "The LKQL script to execute")
        public String script = null;

        @CommandLine.Option(
                names = {"-i", "--interactive"},
                description = "Run a REPL")
        public boolean interactive;

        @CommandLine.Option(
                names = "--keep-going-on-missing-file",
                description = "Keep going on missing file")
        public Boolean keepGoingOnMissingFile = false;

        @Override
        public Integer call() {
            String[] unmatchedArgs;
            if (this.unmatched == null) {
                unmatchedArgs = new String[0];
            } else {
                unmatchedArgs = this.unmatched.toArray(new String[0]);
            }
            new LKQLLauncher(this).launch(unmatchedArgs);
            return 0;
        }
    }

    public LKQLLauncher(LKQLRun args) {
        this.args = args;
    }

    private LKQLRun args = null;

    // ----- Macros and enums -----

    /** The identifier of the LKQL language. */
    private static final String ID = "lkql";

    // ----- Launcher methods -----

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
     * The entry point of the launcher with the context builder.
     *
     * @param contextBuilder The context builder.
     */
    @Override
    protected void launch(Context.Builder contextBuilder) {
        int exitCode = this.executeScript(contextBuilder);
        if (exitCode != 0) {
            throw this.abort((String) null, exitCode);
        }
    }

    /**
     * Execute the LKQL script and return the exit code.
     *
     * @param contextBuilder The context builder.
     * @return The exit code of the script.
     */
    protected int executeScript(Context.Builder contextBuilder) {
        // Create the LKQL options object builder
        final var optionsBuilder = new LKQLOptions.Builder();

        // Set the common configuration
        contextBuilder.allowIO(true);

        // Forward the command line options to the options object builder
        optionsBuilder
                .engineMode(LKQLOptions.EngineMode.INTERPRETER)
                .verbose(this.args.verbose)
                .projectFile(this.args.project)
                .target(this.args.target)
                .runtime(this.args.RTS)
                .keepGoingOnMissingFile(this.args.keepGoingOnMissingFile)
                .files(this.args.files)
                .charset(this.args.charset);

        // Finally, pass the options to the LKQL engine
        contextBuilder.option("lkql.options", optionsBuilder.build().toJson().toString());

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            if (this.args.script != null) {
                final Source source = Source.newBuilder("lkql", new File(this.args.script)).build();
                context.eval(source);
            }

            if (this.args.interactive) {
                LineReader reader = LineReaderBuilder.builder().build();
                String prompt = "> ";
                while (true) {
                    String line = null;
                    try {
                        line = reader.readLine(prompt);
                        final Source source =
                                Source.newBuilder("lkql", line, "<input>")
                                        .interactive(true)
                                        .build();
                        context.eval(source);
                    } catch (UserInterruptException e) {
                        // Ignore
                    } catch (EndOfFileException e) {
                        return 12;
                    } catch (Exception e) {
                        System.err.println(e.getMessage());
                    }
                }
            }

            return 0;
        } catch (IOException e) {
            System.err.println("File not found : " + this.args.script);
            return 2;
        } catch (Exception e) {
            System.err.println(e.getMessage());
            if (this.args.verbose) {
                e.printStackTrace();
            }
            return 0;
        }
    }

    // ----- Argument parsing methods -----

    /**
     * Parse the command line arguments and return the unrecognized options to parse it with the
     * default parser.
     *
     * @param arguments The arguments to parse.
     * @param polyglotOptions The polyglot options.
     * @return The unrecognized options.
     */
    @Override
    protected List<String> preprocessArguments(
            List<String> arguments, Map<String, String> polyglotOptions) {
        if (this.args.unmatched != null) {
            return this.args.unmatched;
        } else {
            return new ArrayList<>();
        }
    }
}
