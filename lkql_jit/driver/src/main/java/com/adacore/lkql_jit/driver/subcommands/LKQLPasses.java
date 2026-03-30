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
import picocli.CommandLine;

public class LKQLPasses extends BaseSubcommand {

    // ----- Attributes -----

    private final Args args;

    // ----- Constructors -----

    public LKQLPasses(Args args) {
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
            .files(this.args.files)
            .charset(this.args.charset);
        this.args.fillGPROptions(optionsBuilder);

        // Finally, pass the options to the LKQL engine
        contextBuilder.option("lkql.options", optionsBuilder.build().toJson().toString());

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
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
                diagnostics.createReport(new TextReportCreator(System.err, supportAnsi));
            }
            return 0;
        } catch (IOException e) {
            System.err.println("File not found : " + this.args.script);
            return 2;
        }
    }

    // ----- Inner classes -----

    @CommandLine.Command(
        name = "run-passes",
        description = "Run the LKQL interpreter on a given script (nanopass mode)"
    )
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

        @CommandLine.Unmatched
        public List<String> unmatched = new ArrayList<>();

        @Override
        public Integer call() {
            new LKQLPasses(this).launch(unmatched.toArray(new String[0]));
            return 0;
        }
    }
}
