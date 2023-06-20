package com.adacore.lkql_jit.utils.util_functions;

import com.adacore.libadalang.Libadalang;
import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
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
     * Common interface for diagnostic emitters. All given parameters need not be used in the output.
     */
    public interface DiagnosticEmitter {
        /**
         * @param ruleName              The name of the rule
         * @param message               The message of the violated rule
         * @param slocRange             The location where the error occurs in the code
         * @param unit                  The analysis unit in which the error occurs
         * @param genericInstantiations The current stack of generic instantiations
         * @param linesCache            The cache of all units' source text lines
         * @param context               The context to output the message
         */
        void emitRuleViolation(
            String ruleName,
            String message,
            Libadalang.SourceLocationRange slocRange,
            Libadalang.AnalysisUnit unit,
            Libadalang.AdaNodeArray genericInstantiations,
            SourceLinesCache linesCache,
            LKQLContext context
        );

        /**
         * @param fromUnit        The unit from which an attempt to retrieve the file was made
         * @param missingFileName The file that failed to be loaded
         * @param isFatal         Whether the error will terminate the execution or not
         * @param context         The context to output the message
         */
        void emitMissingFile(
            Libadalang.AnalysisUnit fromUnit,
            String missingFileName,
            boolean isFatal,
            LKQLContext context
        );

        /**
         * @param ruleName      The name of the rule the error occurred during.
         * @param unit          The analysis unit in which the error occurred.
         * @param adaLocation   The location in the unit of the error.
         * @param errorLocation The location in the LKQL code which rose the exception.
         * @param errorName     The name of the error.
         * @param errorMessage  The message of the error.
         * @param context       The context to output the message.
         */
        void emitInternalError(
            String ruleName,
            Libadalang.AnalysisUnit unit,
            Libadalang.SourceLocation adaLocation,
            String errorLocation,
            String errorName,
            String errorMessage,
            LKQLContext context
        );
    }

    /**
     * The default renderer for emitting diagnostic. Uses colors when possible.
     */
    public static class DefaultEmitter implements DiagnosticEmitter {
        @Override
        @CompilerDirectives.TruffleBoundary
        public void emitRuleViolation(
            String ruleName,
            String message,
            Libadalang.SourceLocationRange slocRange,
            Libadalang.AnalysisUnit unit,
            Libadalang.AdaNodeArray genericInstantiations,
            SourceLinesCache linesCache,
            LKQLContext context
        ) {
            printRuleViolation(
                message,
                slocRange.start.line,
                slocRange.start.column,
                slocRange.end.line,
                slocRange.end.column,
                unit,
                linesCache,
                context
            );
        }

        @Override
        public void emitMissingFile(
            Libadalang.AnalysisUnit fromUnit,
            String missingFileName,
            boolean isFatal,
            LKQLContext context
        ) {
            final String prefix = isFatal ? "ERROR" : "WARNING";
            context.println(prefix + ": File " + FileUtils.baseName(missingFileName) + " not found");
        }

        @Override
        @CompilerDirectives.TruffleBoundary
        public void emitInternalError(
            final String ruleName,
            final Libadalang.AnalysisUnit unit,
            final Libadalang.SourceLocation adaLocation,
            final String errorLocation,
            final String errorName,
            final String errorMessage,
            final LKQLContext context
        ) {
            context.println(unit.getFileName(false) +
                "1:01: internal error: " +
                errorName + ": " + errorMessage + "[" + ruleName.toLowerCase() + "]");
        }
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
     * @param context    The LKQL context to use for the output
     */
    @CompilerDirectives.TruffleBoundary
    public static void printRuleViolation(
        String message,
        int startLine,
        int startCol,
        int endLine,
        int endCol,
        Libadalang.AnalysisUnit unit,
        SourceLinesCache linesCache,
        LKQLContext context
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
        context.println(
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

    /**
     * Emitter that formats diagnostics such that the GNATcheck driver can parse them.
     */
    public static class GNATcheckEmitter implements DiagnosticEmitter {
        @Override
        @CompilerDirectives.TruffleBoundary
        public void emitRuleViolation(
            String ruleName,
            String message,
            Libadalang.SourceLocationRange slocRange,
            Libadalang.AnalysisUnit unit,
            Libadalang.AdaNodeArray genericInstantiations,
            SourceLinesCache linesCache,
            LKQLContext context
        ) {
            // Get the file name
            final String fileName = FileUtils.baseName(unit.getFileName());
            final String colPrefix = slocRange.start.column < 10 ? "0" : "";

            // Append generic instantiation information to the message
            if (genericInstantiations.size() > 0) {
                StringBuilder messageBuilder = new StringBuilder(message);
                for (int i = 0; i < genericInstantiations.size(); ++i) {
                    if (i > 0) {
                        messageBuilder.append(" [");
                    } else {
                        messageBuilder.append(" [instance at ");
                    }
                    final Libadalang.AdaNode inst = genericInstantiations.get(i);
                    messageBuilder.append(FileUtils.baseName(inst.getUnit().getFileName()));
                    messageBuilder.append(":");
                    messageBuilder.append(inst.getSourceLocationRange().start.line);
                }
                messageBuilder.append("]".repeat(genericInstantiations.size()));
                message = messageBuilder.toString();
            }

            // Print the things
            context.println(
                fileName + ":" +
                    slocRange.start.line + ":" +
                    colPrefix + slocRange.start.column + ": " +
                    "check: " +
                    message +
                    " [" + StringUtils.toLowerCase(ruleName) + "]"
            );
        }

        @Override
        public void emitMissingFile(
            Libadalang.AnalysisUnit fromUnit,
            String missingFileName,
            boolean isFatal,
            LKQLContext context
        ) {
            // Use the full name for files: GNATcheck will reformat it if a specific flag is set
            context.println(fromUnit.getFileName() + ":1:1: warning: cannot find " + missingFileName);
        }

        @Override
        @CompilerDirectives.TruffleBoundary
        public void emitInternalError(
            final String ruleName,
            final Libadalang.AnalysisUnit unit,
            final Libadalang.SourceLocation adaLocation,
            final String errorLocation,
            final String errorName,
            final String errorMessage,
            final LKQLContext context
        ) {
            context.println(unit.getFileName(false) + ":" +
                adaLocation.line + ":" + String.format("%02d", adaLocation.column) + ": check: " +
                "internal error at " + errorLocation + ": " +
                "raised " + errorName + " : " +
                errorMessage +
                " [" + ruleName.toLowerCase() + "]"
            );
        }
    }

}
