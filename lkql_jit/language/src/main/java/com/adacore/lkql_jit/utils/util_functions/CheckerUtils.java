package com.adacore.lkql_jit.utils.util_functions;

import com.oracle.truffle.api.CompilerDirectives;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;

import java.util.Arrays;


/**
 * Util functions for the LKQL checker implementation
 *
 * @author Hugo GUERRIER
 */
public class CheckerUtils {

    /**
     * Display a rule violation on the standard output
     *
     * @param message The message of the violated rule
     * @param node The node which violates the rule
     */
    @CompilerDirectives.TruffleBoundary
    public static void printRuleViolation(String message, Libadalang.AdaNode node) {
        Libadalang.SourceLocationRange sourceLocationRange = node.getSourceLocationRange();
        printRuleViolation(
                message,
                sourceLocationRange.start.line,
                sourceLocationRange.start.column,
                sourceLocationRange.end.line,
                sourceLocationRange.end.column,
                node.getUnit()
        );
    }

    /**
     * Display a rule violation on the standard output
     *
     * @param message The message of the violated rule
     * @param token The token that violated the rule
     */
    @CompilerDirectives.TruffleBoundary
    public static void printRuleViolation(String message, Libadalang.Token token) {
        Libadalang.SourceLocationRange sourceLocationRange = token.sourceLocationRange;
        printRuleViolation(
                message,
                sourceLocationRange.start.line,
                sourceLocationRange.start.column,
                sourceLocationRange.end.line,
                sourceLocationRange.end.column,
                token.unit
        );
    }

    /**
     * Display a rule violation on the standard output
     *
     * @param message The message of the violated rule
     * @param startLine The position start line
     * @param startCol The position start column
     * @param endLine The position end line
     * @param endCol The position end column
     * @param unit The analysis unit where the violation happened
     */
    @CompilerDirectives.TruffleBoundary
    public static void printRuleViolation(
            String message,
            int startLine,
            int startCol,
            int endLine,
            int endCol,
            Libadalang.AnalysisUnit unit
    ) {
        // Get the text and the file name
        String text = unit.getText();
        String fileName = FileUtils.baseName(unit.getFileName());

        // Get the valid lines and the source representation
        String[] rawLines = StringUtils.separateLines(text);
        String[] validLines = new String[(endLine + 1) - startLine];
        if (endLine + 1 - startLine >= 0)
            System.arraycopy(rawLines, startLine - 1, validLines, 0, endLine + 1 - startLine);
        String sourceString = StringUtils.underlineSource(
                validLines,
                startLine,
                startCol,
                endLine,
                endCol,
                StringUtils.ANSI_YELLOW
        );

        // Print the things
        System.out.println(
                (LKQLLanguage.SUPPORT_COLOR ? StringUtils.ANSI_BOLD : "") +
                        fileName + ":" +
                        startLine + ":" +
                        startCol + ":" +
                        (LKQLLanguage.SUPPORT_COLOR ? StringUtils.ANSI_YELLOW : "") +
                        " rule violation: " +
                        (LKQLLanguage.SUPPORT_COLOR ? StringUtils.ANSI_RESET : "") +
                        message + "\n" + sourceString + "\n"
        );
    }

}
