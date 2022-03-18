package com.adacore.lkql_jit.utils.util_functions;

import com.oracle.truffle.api.CompilerDirectives;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;


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
        // Get the text and file name
        String text = node.getUnit().getText();
        Libadalang.SourceLocationRange sourceLocationRange = node.getSourceLocationRange();

        int startLine = sourceLocationRange.start.line;
        int endLine = sourceLocationRange.end.line;
        int startCol = sourceLocationRange.start.column;
        int endCol = sourceLocationRange.end.column;
        String fileName = FileUtils.baseName(node.getUnit().getFileName());

        // Get the valid lines and the source representation
        String[] rawLines = StringUtils.split(text, "\n");
        String[] validLines = new String[(endLine + 1) - startLine];
        if (endLine + 1 - startLine >= 0)
            System.arraycopy(rawLines, startLine - 1, validLines, 0, endLine + 1 - startLine);
        String sourceString = StringUtils.underlinedSource(
                validLines,
                startLine,
                endLine,
                startCol,
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
                        message + "\n" + sourceString
        );
    }

}
