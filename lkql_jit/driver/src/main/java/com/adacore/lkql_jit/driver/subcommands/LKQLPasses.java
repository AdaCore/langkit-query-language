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

@CommandLine.Command(
    name = "run-passes",
    description = "Run the LKQL interpreter on a given script (nanopass mode)"
)
public class LKQLPasses extends BaseSubcommand {

    // ----- Attributes -----

    @CommandLine.Mixin
    EngineArgs engineArgs;

    @CommandLine.Mixin
    GPRArgs gprArgs;

    @CommandLine.Spec
    public CommandLine.Model.CommandSpec spec;

    @CommandLine.Parameters(description = "Files to analyze")
    public List<String> files = new ArrayList<>();

    @CommandLine.Option(
        names = { "-S", "--script-path" },
        description = "The LKQL script to execute"
    )
    public String script;

    @CommandLine.Unmatched
    public List<String> unmatched = new ArrayList<>();

    // ----- Constructors -----

    public LKQLPasses() {}

    // ----- Launcher methods -----

    @Override
    public Integer call() {
        launch(unmatched.toArray(new String[0]));
        return 0;
    }

    /** Display the help message for the LKQL language. */
    @Override
    protected void printHelp(OptionCategory maxCategory) {
        spec.commandLine().usage(spec.commandLine().getOut());
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
        return unmatched;
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
            .files(files);
        engineArgs.fillEngineOptions(optionsBuilder);
        gprArgs.fillGPROptions(optionsBuilder);

        // Finally, pass the options to the LKQL engine
        contextBuilder.option("lkql.options", optionsBuilder.build().toJson().toString());

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            try {
                if (script != null) {
                    Source source = Source.newBuilder(Constants.LKQL_ID, new File(script)).build();
                    context.eval(source);
                }
            } catch (PolyglotException e) {
                diagnostics.handleException(e);
                diagnostics.createReport(new TextReportCreator(System.err, supportAnsi));
            }
            return 0;
        } catch (IOException e) {
            System.err.println("File not found : " + script);
            return 2;
        }
    }
}
