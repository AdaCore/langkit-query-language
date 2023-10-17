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
import java.util.ListIterator;
import java.util.Map;
import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

/**
 * This class is the LKQL launcher, this will handle all execution request coming from the command
 * line.
 *
 * @author Hugo GUERRIER
 *     <p>TODO : Support all features of the original LKQL Ada implementation
 */
public class LKQLLauncher extends AbstractLanguageLauncher {

    // ----- Macros and enums -----

    /** Represents the status of an argument. */
    protected enum ArgumentStatus {
        Consumed,
        Unhandled,
        Malformed,
        ExpectInt
    }

    /** The identifier of the LKQL language. */
    private static final String ID = "lkql";

    // ----- Launcher options -----

    /** The charset to decode the LKQL sources. */
    private String charset = null;

    /** The project file to analyse. */
    private String projectFile = null;

    /** Source files to analyse. */
    private final List<String> files = new ArrayList<>();

    /** If the project analysis should be recursive. */
    private boolean recursive = false;

    /** Number of parallel jobs. */
    private int jobs = 0;

    /** The LKQL script to evaluate. */
    private String script = null;

    // ----- JIT options -----

    /** If the verbose mode should be activated. */
    private boolean verbose = false;

    // ----- Launcher methods -----

    /**
     * Display the help message for the LKQL language.
     *
     * @param maxCategory The option category.
     */
    @Override
    protected void printHelp(OptionCategory maxCategory) {
        System.out.println(
                """
                usage : lkql_jit [options ...] files [files ...] [--script|-S SCRIPT_PATH]

                The LKQL JIT compiler

                Positional arguments :
                  files : Files to analyse

                Basic options:
                  --charset, -C                Charset to use for the source decoding
                  --project, -P                Project file to use
                  --recursive, -U              Process all units in the project tree, excluding
                                               externally built projects
                  --jobs, -j                   Number of parallel jobs to use. If zero, use the
                                               maximal parallelism : one job per CPU
                  --script-path, -S            Path of the LKQL script to evaluate

                  --verbose, -v                Enable the verbose mode
                """);
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
     * Start the LKQL launcher.
     *
     * @param args The program args.
     */
    public static void main(String[] args) {
        new LKQLLauncher().launch(args);
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
        if (this.verbose) {
            System.out.println("=== LKQL JIT is in verbose mode ===");
            contextBuilder.option("lkql.verbose", "true");
        }

        // Set the project file
        if (this.projectFile != null) {
            contextBuilder.option("lkql.projectFile", this.projectFile);
        }

        // Set the files
        if (!this.files.isEmpty()) {
            contextBuilder.option("lkql.files", String.join(",", this.files));
        }

        // Set the charset
        if (this.charset != null && !this.charset.isEmpty() && !this.charset.isBlank()) {
            contextBuilder.option("lkql.charset", this.charset);
        }

        // Create the context and run the script in it
        try (Context context = contextBuilder.build()) {
            final Source source = Source.newBuilder("lkql", new File(this.script)).build();
            final Value executable = context.parse(source);
            executable.executeVoid(false);
            return 0;
        } catch (IOException e) {
            System.err.println("File not found : " + this.script);
            return 2;
        } catch (Exception e) {
            if (this.verbose) {
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
        // Prepare the list to return
        final List<String> unrecognizedArguments = new ArrayList<>();

        // Iterate over arguments and parse them
        ListIterator<String> iterator = arguments.listIterator();
        while (iterator.hasNext()) {
            String curArg = iterator.next();

            // Test if the arg is a flag
            if (curArg.startsWith("-") && curArg.length() >= 2 && !curArg.equals("--")) {

                // Get the flag name
                String flag;
                if (curArg.startsWith("--")) {
                    flag = curArg.substring(2);
                } else {
                    flag = this.expandShortFlag(curArg.substring(1));
                }

                // If the flag is not null
                if (flag != null) {

                    // Test if the flag is a solo one
                    if (processFlag(flag) == ArgumentStatus.Consumed) {
                        continue;
                    }

                    // Else try to process it with the value
                    String value;
                    if (iterator.hasNext()) {
                        value = iterator.next();
                    } else {
                        value = null;
                    }

                    // Try to consume the flag
                    switch (this.processFlag(flag, value)) {
                        case Consumed -> {
                            continue;
                        }
                        case Malformed -> throw this.abort("Missing value for " + curArg);
                        case ExpectInt -> throw this.abort("Expected integer value for " + curArg);
                    }

                    // Reset the iterator
                    if (value != null) {
                        iterator.previous();
                    }
                }

                // If all fail, add the argument to the unrecognized arg
                unrecognizedArguments.add(curArg);
            }

            // Else simply add it to the files
            else {
                this.files.add(curArg);
            }
        }

        // Return the list of unrecognized arguments
        return unrecognizedArguments;
    }

    /**
     * Expand a short flag into a long flag.
     *
     * @param shortFlag The short flag.
     * @return The long flag value.
     */
    protected String expandShortFlag(String shortFlag) {
        return switch (shortFlag) {
            case "C" -> "charset";
            case "P" -> "project";
            case "U" -> "recursive";
            case "j" -> "jobs";
            case "S" -> "script-path";
            case "v" -> "verbose";
            default -> null;
        };
    }

    /**
     * Process the flag without argument.
     *
     * @param flag The flag to process.
     * @return The status of the argument parsing.
     */
    protected ArgumentStatus processFlag(String flag) {
        switch (flag) {
                // The recursive flag
            case "recursive":
                this.recursive = true;
                break;

                // The verbose flag
            case "verbose":
                this.verbose = true;
                break;

                // Default behavior
            default:
                return ArgumentStatus.Unhandled;
        }
        return ArgumentStatus.Consumed;
    }

    /**
     * Process a flag with its argument.
     *
     * @param flag The flag to process.
     * @param value The argument value.
     * @return If the flag was consumed, unhandled or wrong.
     */
    protected ArgumentStatus processFlag(String flag, String value) {
        switch (flag) {
                // The charset value
            case "charset":
                if (value == null) {
                    return ArgumentStatus.Malformed;
                }
                this.charset = value;
                break;

                // The project value
            case "project":
                if (value == null) {
                    return ArgumentStatus.Malformed;
                }
                this.projectFile = value;
                break;

                // The jobs value
            case "jobs":
                if (value == null) {
                    return ArgumentStatus.Malformed;
                }
                try {
                    this.jobs = Integer.parseInt(value);
                } catch (Exception e) {
                    return ArgumentStatus.ExpectInt;
                }
                break;

                // The script flag
            case "script-path":
                if (value == null) {
                    return ArgumentStatus.Malformed;
                }
                this.script = value;
                break;

                // The default unhandled flag
            default:
                return ArgumentStatus.Unhandled;
        }
        return ArgumentStatus.Consumed;
    }

    /**
     * Validate the state of the launcher after argument parsing.
     *
     * @param polyglotOptions The polyglot options.
     */
    @Override
    protected void validateArguments(Map<String, String> polyglotOptions) {
        // Launch the super validation
        super.validateArguments(polyglotOptions);

        // Verify the project file
        if (files.isEmpty() && (this.projectFile == null || this.projectFile.isEmpty())) {
            throw this.abort("Please provide files or a project file to analyze");
        }

        // Verify the script file
        if (this.script == null || this.script.isEmpty()) {
            throw this.abort("Please provide a script file to evaluate");
        }
    }
}
