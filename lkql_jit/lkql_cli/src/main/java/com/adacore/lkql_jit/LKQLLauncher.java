/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit;

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
        public List<String> files;

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
        // Set the builder common options
        contextBuilder.allowIO(true);

        // Set the context options
        if (this.args.verbose) {
            System.out.println("=== LKQL JIT is in verbose mode ===");
            contextBuilder.option("lkql.verbose", "true");
        }

        // Set the project file
        if (this.args.project != null) {
            contextBuilder.option("lkql.projectFile", this.args.project);
        }

        if (this.args.RTS != null) {
            contextBuilder.option("lkql.runtime", this.args.RTS);
        }

        if (this.args.target != null) {
            contextBuilder.option("lkql.target", this.args.target);
        }

        if (this.args.keepGoingOnMissingFile) {
            contextBuilder.option("lkql.keepGoingOnMissingFile", "true");
        }

        // Set the files
        if (this.args.files != null) {
            contextBuilder.option("lkql.files", String.join(",", this.args.files));
        }

        // Set the charset
        if (this.args.charset != null
                && !this.args.charset.isEmpty()
                && !this.args.charset.isBlank()) {
            contextBuilder.option("lkql.charset", this.args.charset);
        }

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
            if (this.args.verbose) {
                e.printStackTrace();
            } else {
                System.err.println(e.getMessage());
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
            return new ArrayList<String>();
        }
    }
}
