package com.adacore.lkql_jit.utils.util_functions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.oracle.truffle.api.CompilerDirectives;
import org.graalvm.collections.EconomicMap;


/**
 * Util functions for the LKQL checker implementation
 *
 * @author Hugo GUERRIER
 */
public class CheckerUtils {
    /**
     * Caches source lines of analysis units to avoid recomputing them each time a diagnostic needs to be emitted.
     */
    public static class SourceLinesCache {
        private final EconomicMap<Libadalang.AdaNode, String[]> sourcesLines = EconomicMap.create();

        /**
         * Return the lines of code composing the given analysis unit as an array of Strings.
         * This either fetches them from the cache if they were already computed previously, or computes them
         * and stores them in the cache for later reuse.
         *
         * @param unit The unit from which to extract source lines
         */
        @CompilerDirectives.TruffleBoundary
        private String[] getLines(Libadalang.AnalysisUnit unit) {
            final Libadalang.AdaNode root = unit.getRoot();
            String[] result = sourcesLines.get(root, null);
            if (result == null) {
                result = StringUtils.separateLines(unit.getText());
                sourcesLines.put(root, result);
            }
            return result;
        }
    }

    /**
     * Display a rule violation on the standard output
     *
     * @param message    The message of the violated rule
     * @param slocRange  The location where the error occurs in the code
     * @param unit       The analysis unit in which the error occurs
     * @param linesCache The cache of all units' source text lines
     */
    @CompilerDirectives.TruffleBoundary
    public static void printRuleViolation(
        String message,
        Libadalang.SourceLocationRange slocRange,
        Libadalang.AnalysisUnit unit,
        SourceLinesCache linesCache
    ) {
        printRuleViolation(
            message,
            slocRange.start.line,
            slocRange.start.column,
            slocRange.end.line,
            slocRange.end.column,
            unit,
            linesCache
        );
    }

    /**
     * Display a rule violation on the standard output
     *
     * @param message    The message of the violated rule
     * @param startLine  The position start line
     * @param startCol   The position start column
     * @param endLine    The position end line
     * @param endCol     The position end column
     * @param unit       The analysis unit where the violation happened
     * @param linesCache The cache of all units' source text lines
     */
    @CompilerDirectives.TruffleBoundary
    public static void printRuleViolation(
        String message,
        int startLine,
        int startCol,
        int endLine,
        int endCol,
        Libadalang.AnalysisUnit unit,
        SourceLinesCache linesCache
    ) {
        // Get the file name
        String fileName = FileUtils.baseName(unit.getFileName());

        // Get the valid lines and the source representation
        String[] sourceLines = linesCache.getLines(unit);
        String[] validLines = new String[(endLine + 1) - startLine];
        if (endLine + 1 - startLine >= 0)
            System.arraycopy(sourceLines, startLine - 1, validLines, 0, endLine + 1 - startLine);

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
