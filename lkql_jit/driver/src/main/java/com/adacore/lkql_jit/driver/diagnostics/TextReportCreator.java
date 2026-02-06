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

/** This class provide a callable interface to create a text report from a diagnostic collector. */
public final class TextReportCreator implements Consumer<BaseDiagnostic> {

    // ----- Attributes -----

    // --- ANSI color codes
    private static final String ANSI_RESET = "\u001B[0m";
    private static final String ANSI_BLUE = "\u001B[34m";
    private static final String ANSI_YELLOW = "\u001B[33m";
    private static final Object ANSI_RED = "\u001B[31m";
    private static final String ANSI_BOLD = "\u001B[1m";

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
        var locationName = diagnostic.location.map(
            l -> l.getSourceName() + ":" + l.startLine() + ":" + l.startColumn() + ": "
        );
        var kindColor = switch (diagnostic) {
            case Warning _ -> ANSI_YELLOW;
            case Error _ -> ANSI_RED;
            case RuleViolation _ -> ANSI_YELLOW;
            case RawMessage _ -> throw new RuntimeException("Shouldn't reach here");
        };
        var kindName = switch (diagnostic) {
            case Warning _ -> "warning";
            case Error _ -> "error";
            case RuleViolation _ -> "rule violation";
            case RawMessage _ -> throw new RuntimeException("Shouldn't reach here");
        };

        // Then output the diagnostic
        if (withColors) output.print(ANSI_BOLD);
        locationName.ifPresent(output::print);
        if (withColors) output.print(kindColor);
        output.print(kindName + ": ");
        if (withColors) output.print(ANSI_RESET);
        output.println(diagnostic.message);
        diagnostic.location.ifPresent(l -> printSourceSnippet(l, ANSI_YELLOW, 0));
    }

    /**
     * Get a string representing this source location as a source snippet, with the section
     * underlined with the required color.
     * Pad the result with space characters following the provided amount.
     */
    private void printSourceSnippet(
        SourceSection location,
        String underlineColor,
        int leftPadding
    ) {
        // Get lines and compute the size of the line number colon
        var colSize = String.valueOf(location.endLine()).length();
        var lines = location.getLines();

        // Create the function to start a line
        Consumer<Integer> startLine = lineNum -> {
            // Pad the line start
            output.print(" ".repeat(leftPadding) + (withColors ? ANSI_BLUE : ""));

            // If a line number has been provided then display it, otherwise fill with spaces
            if (lineNum != null) {
                var lineNumStr = String.valueOf(lineNum);
                output.print(lineNumStr);
                output.print(" ".repeat(colSize - lineNumStr.length()));
            } else {
                output.print(" ".repeat(colSize));
            }

            // Finally display the separator
            output.print(" |" + (withColors ? ANSI_RESET : ""));
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
            if (withColors) output.print(underlineColor);
            output.print(" ".repeat(location.startColumn()));
            output.print(
                "^".repeat(Math.max(0, location.endColumn() - location.startColumn() + 1))
            );
        }
        // Else do the multiline display
        else {
            // Print the first source line with the underlining
            output.println("  " + lines.getFirst());
            startLine.accept(null);
            output.println(
                (withColors ? underlineColor : "") + ' ' + "_".repeat(location.startColumn()) + '^'
            );

            // Then show a message to tell how many lines have been skipped if there are more than 1
            if (lines.size() > 2) {
                startLine.accept(null);
                output.println('|');
                startLine.accept(null);
                output.printf("| ~~~ %d other lines ~~~%n", lines.size() - 2);
                startLine.accept(null);
                output.println('|');
            }

            // Finally show the final line of the section with the underlining end
            startLine.accept(location.endLine());
            output.println("| " + (withColors ? ANSI_RESET : "") + lines.getLast());
            startLine.accept(null);
            if (withColors) output.print(underlineColor);
            output.print('|');
            output.print("_".repeat(Math.max(1, location.endColumn())));
            output.print('^');
        }

        // Finally reset ANSI colors
        if (withColors) output.print(ANSI_RESET);
        output.println();
        output.println();
    }
}
