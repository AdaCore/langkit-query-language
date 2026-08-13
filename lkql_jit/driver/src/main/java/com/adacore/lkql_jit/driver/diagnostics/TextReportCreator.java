//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.Styling;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.diagnostics.variants.Exception;
import com.adacore.lkql_jit.driver.diagnostics.variants.*;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.io.PrintStream;
import java.util.function.Consumer;

/** This class provide a callable interface to create a text report from a diagnostic collector. */
public final class TextReportCreator implements Consumer<BaseDiagnostic> {

    // ----- Attributes -----

    /** Stream to output the report in. */
    private final PrintStream output;

    /** Whether to emit ANSI code to style the report. */
    private final boolean withStyle;

    // ----- Constructors -----

    public TextReportCreator(PrintStream output, boolean withStyle) {
        this.output = output;
        this.withStyle = withStyle;
    }

    // ----- Instance methods ------

    @Override
    public void accept(BaseDiagnostic diagnostic) {
        // Create variant part from the diagnostic information
        var locationName = diagnostic.location.map(l -> l.shortImage() + ": ");
        Styling.StylingFunction kindStyle = switch (diagnostic) {
            case Error _, Exception _ -> Styling::red;
            case Warning _, RuleViolation _ -> Styling::yellow;
            case Info _ -> Styling::brightBlue;
        };
        var kindName = switch (diagnostic) {
            case Info _ -> "info";
            case Warning _ -> "warning";
            case Error _, Exception _ -> "error";
            case RuleViolation _ -> "rule violation";
        };
        var leftPadding = switch (diagnostic) {
            case RuleViolation _ -> 0;
            default -> 2;
        };

        // Then output the diagnostic
        output.print(styled(locationName.orElse(""), Styling::bold));
        output.print(styled(kindName + ": ", Styling::bold, kindStyle));
        output.println(diagnostic.message);
        diagnostic.location.ifPresent(l -> printSourceSnippet(l, Styling::yellow, leftPadding));

        // In the case of an exception, show the call stack if there is one
        if (diagnostic instanceof Exception exception) {
            for (var frame : exception.callStack) {
                output.print(styled(frame.locationImage() + ": ", Styling::bold));
                output.print("in ");
                output.println(styled(frame.callContext(), Styling::bold, Styling::red));
                if (frame instanceof Exception.CustomFrame f) printSourceSnippet(
                    f.callLocation,
                    Styling::yellow,
                    2
                );
            }
        }

        // If there are some hints, display them
        for (var hint : diagnostic.hints) {
            var hintLocationImage = hint.location.map(l -> l.shortImage() + ": ");
            output.print(styled(hintLocationImage.orElse(""), Styling::bold));
            output.print(styled("hint: ", Styling::blue, Styling::bold));
            output.println(hint.message);
            hint.location.ifPresent(l -> printSourceSnippet(l, Styling::blue, 2));
        }

        // If there are auto-fixes, display them
        if (!diagnostic.autoFixes.isEmpty()) {
            output.println(styled("fix proposal:", Styling::bold, Styling::brightGreen));
            for (var autoFix : diagnostic.autoFixes) {
                output.println(
                    "  in " +
                        styled(
                            autoFix.targetSource().getName(),
                            Styling::bold,
                            Styling::underline
                        ) +
                        ":"
                );
                output.println(autoFix.toPrettyString("  ", 2, withStyle));
            }
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
        Styling.StylingFunction underlineStyle,
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
                output.print(
                    styled(lineNumStr + " ".repeat(colSize - lineNumStr.length()), Styling::blue)
                );
            } else {
                output.print(styled(" ".repeat(colSize), Styling::blue));
            }

            // Finally display the separator
            output.print(styled(" |", Styling::blue));
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
                styled(
                    "^".repeat(Math.max(0, location.endColumn() - location.startColumn())),
                    underlineStyle
                )
            );
        }
        // Else do the multiline display
        else {
            // Print the first source line with the underlining
            output.println("  " + lines.getFirst());
            startLine.accept(null);
            output.println(styled(' ' + "_".repeat(location.startColumn()) + '^', underlineStyle));

            // Then show a message to tell how many lines have been skipped if there are more than 1
            if (lines.size() > 2) {
                startLine.accept(null);
                output.println(styled("|", underlineStyle));
                startLine.accept(null);
                output.printf(
                    styled("| ~~~ %d other lines ~~~%n", underlineStyle),
                    lines.size() - 2
                );
                startLine.accept(null);
                output.println(styled("|", underlineStyle));
            }

            // Finally show the final line of the section with the underlining end
            startLine.accept(location.endLine());
            output.println(styled("| ", underlineStyle) + lines.getLast());
            startLine.accept(null);
            output.println(
                styled(
                    '|' + "_".repeat(Math.max(1, location.endColumn() - 1)) + '^',
                    underlineStyle
                )
            );
        }
    }

    /** Inner helper to dispatch text style emission. */
    private String styled(String s, Styling.StylingFunction style) {
        return withStyle ? style.apply(s) : s;
    }

    /** Inner helper to compose some styles. */
    private String styled(String s, Styling.StylingFunction... styles) {
        if (withStyle) {
            for (var style : styles) {
                s = style.apply(s);
            }
        }
        return s;
    }
}
