//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.driver.diagnostics.TextReportCreator;
import com.adacore.lkql_jit.options.LKQLOptions;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.graalvm.shadowed.org.jline.reader.EndOfFileException;
import org.graalvm.shadowed.org.jline.reader.LineReader;
import org.graalvm.shadowed.org.jline.reader.LineReaderBuilder;
import org.graalvm.shadowed.org.jline.reader.UserInterruptException;
import org.graalvm.shadowed.org.jline.terminal.TerminalBuilder;
import picocli.CommandLine;

/**
 * This class is the LKQL launcher, this will handle all execution request coming from the command
 * line.
 */
public class LKQLRun extends BaseSubcommand {

    // ----- Attributes -----

    private final Args args;

    // ----- Constructors -----

    public LKQLRun(Args args) {
        this.args = args;
    }

    // ----- Launcher methods -----

    /** Display the help message for the LKQL language. */
    @Override
    protected void printHelp(OptionCategory maxCategory) {
        this.args.spec.commandLine().usage(this.args.spec.commandLine().getOut());
    }

    /** Simply return the language id. */
    @Override
    protected String getLanguageId() {
        return Constants.LKQL_ID;
    }

    /**
     * Parse the command line arguments and return the unrecognized options to parse it with the
     * default parser.
     */
    @Override
    protected List<String> preprocessArguments(
        List<String> arguments,
        Map<String, String> polyglotOptions
    ) {
        return args.unmatched;
    }

    /** The entry point of the launcher with the context builder. */
    @Override
    protected void launch(Context.Builder contextBuilder) {
        int exitCode = this.executeScript(contextBuilder);
        if (exitCode != 0) {
            throw this.abort((String) null, exitCode);
        }
    }

    /** Execute the LKQL script and return the exit code. */
    protected int executeScript(Context.Builder contextBuilder) {
        // Set the common configuration
        contextBuilder.allowIO(IOAccess.ALL).logHandler(logHandler);

        // Forward the command line options to the options builder
        var optionsBuilder = new LKQLOptions.Builder()
            .engineMode(LKQLOptions.EngineMode.INTERPRETER)
            .verbose(this.args.verbose)
            .missingFileIsError(this.args.missingFileIsError)
            .files(this.args.files)
            .charset(this.args.charset);
        this.args.fillGPROptions(optionsBuilder);

        // Finally, pass the options to the LKQL engine
        contextBuilder.option("lkql.options", optionsBuilder.build().toJson().toString());

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            // If a script has been provided, run it and handle possible exceptions
            try {
                if (this.args.script != null) {
                    Source source = Source.newBuilder(
                        Constants.LKQL_ID,
                        new File(this.args.script)
                    ).build();
                    context.eval(source);
                }
            } catch (PolyglotException e) {
                diagnostics.handleException(e);
            }

            // If an error occurred, display it and exit
            diagnostics.createReport(new TextReportCreator(System.err, supportAnsi));
            if (diagnostics.hasError()) {
                return 0;
            }
            diagnostics.clear();

            // Then, if the user required an interactive session, start it
            if (this.args.interactive) {
                LineReader reader = LineReaderBuilder.builder()
                    .terminal(TerminalBuilder.builder().system(true).dumb(true).build())
                    .build();
                String prompt = "> ";
                while (true) {
                    String line;
                    try {
                        line = reader.readLine(prompt);
                        final Source source = Source.newBuilder(Constants.LKQL_ID, line, "<input>")
                            .interactive(true)
                            .build();
                        context.eval(source);
                    } catch (UserInterruptException e) {
                        // Ignore
                    } catch (EndOfFileException e) {
                        return 12;
                    } catch (PolyglotException e) {
                        diagnostics.handleException(e);
                        diagnostics.createReport(new TextReportCreator(System.err, supportAnsi));
                        diagnostics.clear();
                    }
                }
            }

            // Finally return the success exit code
            return 0;
        } catch (IOException e) {
            System.err.println("File not found : " + this.args.script);
            return 2;
        }
    }

    // ----- Inner classes -----

    @CommandLine.Command(name = "run", description = "Run the LKQL interpreter on a given script")
    public static class Args extends GPRArgs {

        @CommandLine.Spec
        public CommandLine.Model.CommandSpec spec;

        @CommandLine.Parameters(description = "Files to analyze")
        public List<String> files = new ArrayList<>();

        @CommandLine.Option(
            names = { "-C", "--charset" },
            description = "Charset to use for the source decoding"
        )
        public String charset = null;

        @CommandLine.Option(names = { "-v", "--verbose" }, description = "Enable the verbose mode")
        public boolean verbose;

        @CommandLine.Option(
            names = { "-S", "--script-path" },
            description = "The LKQL script to execute"
        )
        public String script = null;

        @CommandLine.Option(names = { "-i", "--interactive" }, description = "Run a REPL")
        public boolean interactive;

        @CommandLine.Option(
            names = "--missing-file-is-error",
            description = "Consider and log missing files as errors"
        )
        public Boolean missingFileIsError = false;

        @CommandLine.Unmatched
        public List<String> unmatched = new ArrayList<>();

        @Override
        public Integer call() {
            new LKQLRun(this).launch(unmatched.toArray(new String[0]));
            return 0;
        }
    }
}
