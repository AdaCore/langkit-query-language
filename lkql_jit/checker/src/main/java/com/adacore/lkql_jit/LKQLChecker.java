/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit;

import org.graalvm.launcher.AbstractLanguageLauncher;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;


/**
 * This class represents the LKQL checker entry point with the LKQL JIT backend
 * This is a TEMPORARY driver to perform efficiency tests on LKQL JIT in real life use case
 * TODO : Support all flags and options of the lkql_checker original implementation
 *
 * @author Hugo GUERRIER
 */
public class LKQLChecker extends AbstractLanguageLauncher {

    // ----- Macros and enums -----

    /**
     * Represents the status of an argument
     */
    protected enum ArgumentStatus {
        Consumed,
        Unhandled,
        Malformed,
        ExpectInt
    }

    /**
     * The identifier of the LKQL language
     */
    private static final String ID = "lkql";

    // ----- Launcher options -----

    /**
     * The charset to decode the LKQL sources
     */
    private String charset = null;

    /**
     * The project file to analyse
     */
    private String projectFile = null;

    /**
     * Source files to analyse
     */
    private final List<String> files = new ArrayList<>();

    /**
     * If the project analysis should be recursive
     */
    private boolean recursive = false;

    /**
     * Number of parallel jobs
     */
    private int jobs = 0;

    /**
     * If the verbose mode should be activated
     */
    private boolean verbose = false;

    // ----- Checker options -----

    /**
     * A directory containing all user added rules
     */
    private String rulesDirs = null;

    /**
     * The rules to apply
     */
    private String rules = null;

    /**
     * The arguments to pass to the rules
     */
    private List<String> rulesArgs = new ArrayList<>();

    /**
     * The source files to ignore during analysis
     */
    private String ignores = null;

    /**
     * The mode of error recovery
     */
    private final String errorMode = "continue_and_warn";

    // ----- Checker methods -----

    /**
     * Display the help message for the LKQL language
     *
     * @param maxCategory The option category
     */
    @Override
    protected void printHelp(OptionCategory maxCategory) {
        System.out.println(
            """
                usage : lkql_jit_checker [options ...] files [files ...]

                The LKQL checker using LKQL JIT compiler

                Positional arguments :
                  files : Files to analyse

                Basic options:
                  --charset, -C                     Charset to use for the source decoding
                  --project, -P                     Project file to use
                  --recursive, -U                   Process all units in the project tree, excluding
                                                    externally built projects
                  --jobs, -j                        Number of parallel jobs to use. If zero, use the
                                                    maximal parallelism : one job per CPU

                  --verbose, -v                     Enable the verbose mode

                  --rules-dirs                      Specify directories where rules will be seek from
                  --rules, -r                       Comma separated rules to apply (if not passed
                                                    all rules are applied)
                  --rule-arg, -a                    Argument to pass to a rule, with the syntax
                                                    <rule_name>.<arg_name>=<arg_value>
                  --property-error-recovery, -pr    Which behavior to adopt when there is a
                                                    property error inside of a LKQL query.
                                                    Possible alternatives: continue_and_warn,
                                                    continue_and_log, raise_error. Default:
                                                    continue_and_warn
                """
        );
    }

    /**
     * Simply return the language id
     *
     * @return The language id
     */
    @Override
    protected String getLanguageId() {
        return ID;
    }

    /**
     * Start the LKQL checker
     *
     * @param args The params
     */
    public static void main(String[] args) {
        new LKQLChecker().launch(args);
    }

    /**
     * Start the LQKL checker
     *
     * @param contextBuilder The context builder to build LKQL context
     */
    @Override
    protected void launch(Context.Builder contextBuilder) {
        int exitCode = this.executeScript(contextBuilder);
        if (exitCode != 0) {
            throw this.abort((String) null, exitCode);
        }
    }

    /**
     * Execute the LKQL checker script and return the exit code
     *
     * @param contextBuilder The context builder
     * @return The exit code of the script
     */
    protected int executeScript(Context.Builder contextBuilder) {
        // Set the builder common options
        contextBuilder.allowIO(true);

        // Set the LKQL language mode to interpreter
        contextBuilder.option("lkql.checkerMode", "true");

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
            contextBuilder.option("lkql.files", String.join(File.pathSeparator, this.files));
        }

        // Set the charset
        if (this.charset != null && !this.charset.isEmpty() && !this.charset.isBlank()) {
            contextBuilder.option("lkql.charset", this.charset);
        }

        // Set the rule directories
        if (this.rulesDirs != null) {
            contextBuilder.option("lkql.rulesDirs", this.rulesDirs);
        }

        // Set the rule to apply
        if (this.rules != null && !this.rules.isEmpty() && !this.rules.isBlank()) {
            contextBuilder.option("lkql.rules", this.rules.toLowerCase());
        }

        // Set the rule argument
        contextBuilder.option("lkql.rulesArgs", String.join(";", this.rulesArgs));

        // Set the Ada files to ignore during the analysis
        if (this.ignores != null && !this.ignores.isEmpty() && !this.ignores.isBlank()) {
            contextBuilder.option("lkql.ignores", this.ignores);
        }

        try (Context context = contextBuilder.build()) {
            // Create the context and run it with the script
            Source source = Source.newBuilder("lkql", checkerSource, "checker.lkql")
                .build();
            context.eval(source);

            // Return the success
            return 0;
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
     * Get the checker specific argument and return the unparsed ones to the default parser
     *
     * @param arguments       The arguments to parse
     * @param polyglotOptions The polyglot options
     * @return The unrecognized options
     */
    @Override
    protected List<String> preprocessArguments(List<String> arguments, Map<String, String> polyglotOptions) {
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

        // Return the unhandled arguments
        return unrecognizedArguments;
    }

    /**
     * Expand a short flag into a long flag
     *
     * @param shortFlag The short flag
     * @return The long flag value
     */
    protected String expandShortFlag(String shortFlag) {
        return switch (shortFlag) {
            case "C" -> "charset";
            case "P" -> "project";
            case "U" -> "recursive";
            case "j" -> "jobs";
            case "v" -> "verbose";
            case "r" -> "rules";
            case "a" -> "rule-arg";
            default -> null;
        };
    }

    /**
     * Process the flag without argument
     *
     * @param flag The flag to process
     * @return The status of the argument parsing
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
     * Process a flag with its argument
     *
     * @param flag  The flag to process
     * @param value The argument value
     * @return If the flag was consumed, unhandled or wrong
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

            // The add rule dir
            case "rules-dirs":
                if (value == null) {
                    return ArgumentStatus.Malformed;
                }
                this.rulesDirs = value;
                break;

            // The rule precision
            case "rules":
                if (value == null) {
                    return ArgumentStatus.Malformed;
                }
                this.rules = value;
                break;

            case "rule-arg":
                if (value == null) {
                    return ArgumentStatus.Malformed;
                }
                this.rulesArgs.add(value);
                break;

            case "ignores":
                if (value == null) {
                    return ArgumentStatus.Malformed;
                }
                this.ignores = value;
                break;

            // The default unhandled flag
            default:
                return ArgumentStatus.Unhandled;
        }
        return ArgumentStatus.Consumed;
    }

    /**
     * Validate the state of the launcher after argument parsing
     *
     * @param polyglotOptions The polyglot options
     */
    @Override
    protected void validateArguments(Map<String, String> polyglotOptions) {
        // Launch the super validation
        super.validateArguments(polyglotOptions);

        // Verify the project file
        if (this.files.isEmpty() && (this.projectFile == null || this.projectFile.isEmpty())) {
            System.err.println("No source file to process");
        }
    }

    // ----- The LKQL checker -----

    public static final String checkerSource =
        """
            val unts = units()
            val roots = [unit.root for unit in unts]

            map(roots, (root) => node_checker(root))
            map(unts, (unit) => unit_checker(unit))
            """;

}
