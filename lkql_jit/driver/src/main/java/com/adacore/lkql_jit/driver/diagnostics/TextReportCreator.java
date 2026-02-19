//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.diagnostics.variants.*;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.io.PrintStream;
import java.util.function.Consumer;
import java.util.function.Function;

/** This class provide a callable interface to create a text report from a diagnostic collector. */
public final class TextReportCreator implements Consumer<BaseDiagnostic> {

    // ----- Attributes -----

    /** Stream to output the report in. */
    private final PrintStream output;

    /** Whether to include ANSI colors in the report */
    private final boolean withColors;

    // ----- Constructors -----

    public TextReportCreator(PrintStream output, boolean withColors) {
        this.output = output;
        this.withColors = withColors;
    }

    // ----- Instance methods ------

    @Override
    public void accept(BaseDiagnostic diagnostic) {
        // If the diagnostic is a raw message, just display its content and do nothing else
        if (diagnostic instanceof RawMessage rawMessage) {
            output.println(rawMessage.message);
            return;
        }

        // Create variant part from the diagnostic information
        var locationName = diagnostic.location.map(l -> l.shortImage() + ": ");
        StylingFunction kindStyle = switch (diagnostic) {
            case Error _ -> this::red;
            case Warning _, RuleViolation _ -> this::yellow;
            case RawMessage _ -> throw new RuntimeException("Shouldn't reach here");
        };
        var kindName = switch (diagnostic) {
            case Warning _ -> "warning";
            case Error _ -> "error";
            case RuleViolation _ -> "rule violation";
            case RawMessage _ -> throw new RuntimeException("Shouldn't reach here");
        };

        // Then output the diagnostic
        output.print(bold(locationName.orElse("") + kindStyle.apply(kindName + ": ")));
        output.println(diagnostic.message);
        diagnostic.location.ifPresent(l -> printSourceSnippet(l, this::yellow, 0));

        // In the case of an error, show the call stack if there is one
        if (diagnostic instanceof Error error) {
            for (var call : error.callStack) {
                output.print(bold(call.callLocation().shortImage() + ": "));
                output.print("in ");
                output.println(bold(red(call.callContext())));
                printSourceSnippet(call.callLocation(), this::yellow, 2);
            }
        }

        // If there are some hints, display them
        for (var hint : diagnostic.hints) {
            var hintLocationImage = hint.location.map(l -> l.shortImage() + ": ");
            output.print(bold(hintLocationImage.orElse("") + blue("hint: ")));
            output.println(hint.message);
            hint.location.ifPresent(l -> printSourceSnippet(l, this::blue, 0));
        }

        // Display a final newline
        output.println();
    }

    /**
     * Get a string representing this source location as a source snippet, with the section
     * underlined with the required color.
     * Pad the result with space characters following the provided amount.
     */
    private void printSourceSnippet(
        SourceSection location,
        StylingFunction underlineStyle,
        int leftPadding
    ) {
        // Get lines and compute the size of the line number colon
        var colSize = String.valueOf(location.endLine()).length();
        var lines = location.getLines();

        // Create the function to start a line
        Consumer<Integer> startLine = lineNum -> {
            // Pad the line start
            output.print(" ".repeat(leftPadding));

            // If a line number has been provided then display it, otherwise fill with spaces
            if (lineNum != null) {
                var lineNumStr = String.valueOf(lineNum);
                output.print(blue(lineNumStr + " ".repeat(colSize - lineNumStr.length())));
            } else {
                output.print(blue(" ".repeat(colSize)));
            }

            // Finally display the separator
            output.print(blue(" |"));
        };

        // Always show the first line number
        startLine.accept(location.startLine());

        // If the source is single line
        if (lines.size() == 1) {
            // Show the line with its number
            output.print(' ');
            output.println(lines.getFirst());

            // Then underline the source section
            startLine.accept(null);
            output.print(" ".repeat(location.startColumn()));
            output.println(
                underlineStyle.apply(
                    "^".repeat(Math.max(0, location.endColumn() - location.startColumn() + 1))
                )
            );
        }
        // Else do the multiline display
        else {
            // Print the first source line with the underlining
            output.println("  " + lines.getFirst());
            startLine.accept(null);
            output.println(underlineStyle.apply(' ' + "_".repeat(location.startColumn()) + '^'));

            // Then show a message to tell how many lines have been skipped if there are more than 1
            if (lines.size() > 2) {
                startLine.accept(null);
                output.println(underlineStyle.apply("|"));
                startLine.accept(null);
                output.printf(underlineStyle.apply("| ~~~ %d other lines ~~~%n"), lines.size() - 2);
                startLine.accept(null);
                output.println(underlineStyle.apply("|"));
            }

            // Finally show the final line of the section with the underlining end
            startLine.accept(location.endLine());
            output.println(underlineStyle.apply("| ") + lines.getLast());
            startLine.accept(null);
            output.println(
                underlineStyle.apply('|' + "_".repeat(Math.max(1, location.endColumn())) + '^')
            );
        }
    }

    /**
     * This interface defines a function that can be called to color a text with an ANSI sequence.
     */
    private interface StylingFunction extends Function<String, String> {}

    private String bold(String s) {
        return styled(s, "\u001B[1m");
    }

    private String red(String s) {
        return styled(s, "\u001B[31m");
    }

    private String blue(String s) {
        return styled(s, "\u001B[34m");
    }

    private String yellow(String s) {
        return styled(s, "\u001B[33m");
    }

    private String styled(String s, String ansiStyle) {
        return withColors ? ansiStyle + s + "\u001B[0m" : s;
    }
}
