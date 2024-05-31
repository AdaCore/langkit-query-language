//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.checker.utils;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.checker.BaseChecker;
import com.adacore.lkql_jit.utils.functions.FileUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.CompilerDirectives;
import org.graalvm.collections.EconomicMap;

/**
 * Util functions for the LKQL checker implementation.
 *
 * @author Hugo GUERRIER
 */
public class CheckerUtils {

    /**
     * Caches source lines of analysis units to avoid recomputing them each time a diagnostic needs
     * to be emitted.
     */
    public static class SourceLinesCache {
        private final EconomicMap<Libadalang.AdaNode, String[]> sourcesLines = EconomicMap.create();

        /**
         * Return the lines of code composing the given analysis unit as an array of Strings. This
         * either fetches them from the cache if they were already computed previously, or computes
         * them and stores them in the cache for later reuse.
         *
         * @param unit The unit from which to extract source lines
         */
        @CompilerDirectives.TruffleBoundary
        public String[] getLines(Libadalang.AnalysisUnit unit) {
            final Libadalang.AdaNode root = unit.getRoot();
            String[] result = sourcesLines.get(root, null);
            if (result == null) {
                result = StringUtils.separateLines(unit.getText());
                sourcesLines.put(root, result);
            }
            return result;
        }
    }

    public static enum MessageKind {
        WARNING,
        ERROR,
        RULE_VIOLATION
    }

    /**
     * Common interface for diagnostic emitters. All given parameters need not be used in the
     * output.
     */
    public interface DiagnosticEmitter {

        /** Emit a rule violation TODO: Meld that into emitDiagnostic eventually */
        void emitRuleViolation(
                BaseChecker checker,
                String message,
                SourceLocation violationLocation,
                Libadalang.AdaNode[] genericInstantiations,
                LKQLContext context);

        /**
         * Main method to be overridden by implementing classes. Produces the diagnostic as a string
         * and return it. Diagnostics can be emitted for Ada code, LKQL code, or both, depending on
         * the location parameters.
         *
         * @param messageKind The kind of the diagnostic to emit
         * @param message The message for the diagnostic
         * @param adaErrorLocation The location in Ada code, if any, else null
         * @param lkqlErrorLocation The location in LKQL code, if any, else null
         * @param ruleName The name of the associated LKQL rule, if any, else ""
         * @return The diagnostic as a string
         */
        String diagnostic(
                MessageKind messageKind,
                String message,
                SourceLocation adaErrorLocation,
                SourceLocation lkqlErrorLocation,
                String ruleName);

        /**
         * Emit a diagnostic. Location parameters can be null. If both are null, then a non located
         * diagnostic will be emitted. The ruleName can also be "", in which case the diagnostic
         * will not be associated to a rule.
         */
        @CompilerDirectives.TruffleBoundary
        default void emitDiagnostic(
                MessageKind messageKind,
                String errorMessage,
                SourceLocation adaErrorLocation,
                SourceLocation lkqlErrorLocation,
                String ruleName) {
            LKQLLanguage.getContext(null)
                    .println(
                            this.diagnostic(
                                    messageKind,
                                    errorMessage,
                                    adaErrorLocation,
                                    lkqlErrorLocation,
                                    ruleName));
        }

        /** Shortcut to emit a diagnostic with no rule. */
        default void emitDiagnostic(
                MessageKind messageKind,
                String errorMessage,
                SourceLocation adaErrorLocation,
                SourceLocation lkqlErrorLocation) {
            emitDiagnostic(messageKind, errorMessage, adaErrorLocation, lkqlErrorLocation, "");
        }

        /**
         * Default mapping from MessageKind to string. Can be overriden by subclasses to alter the
         * way the message is formatted.
         */
        default String kindtoString(MessageKind messageKind) {
            return switch (messageKind) {
                case WARNING -> "warning";
                case ERROR -> "error";
                case RULE_VIOLATION -> "rule violation";
            };
        }

        /** Shortcut to emit a "file not found" message. */
        default void emitFileNotFound(SourceLocation from, String fileName, boolean isError) {

            this.emitDiagnostic(
                    isError ? MessageKind.ERROR : MessageKind.WARNING,
                    "File "
                            + (useFullFilePath() ? fileName : FileUtils.baseName(fileName))
                            + " not found",
                    from,
                    null,
                    "");
        }

        default boolean useFullFilePath() {
            return false;
        }
    }

    /** The default renderer for emitting diagnostic. Uses colors when possible. */
    public static class DefaultEmitter implements DiagnosticEmitter {
        @Override
        @CompilerDirectives.TruffleBoundary
        public void emitRuleViolation(
                BaseChecker checker,
                String message,
                SourceLocation violationLocation,
                Libadalang.AdaNode[] genericInstantiations,
                LKQLContext context) {

            // Print the things
            context.println(
                    "%s%s:%s rule violation: %s%s\n%s\n"
                            .formatted(
                                    (LKQLLanguage.SUPPORT_COLOR ? StringUtils.ANSI_BOLD : ""),
                                    violationLocation.display(),
                                    (LKQLLanguage.SUPPORT_COLOR ? StringUtils.ANSI_YELLOW : ""),
                                    (LKQLLanguage.SUPPORT_COLOR ? StringUtils.ANSI_RESET : ""),
                                    message,
                                    StringUtils.underlineSource(
                                            violationLocation, StringUtils.ANSI_YELLOW)));
        }

        @Override
        public String diagnostic(
                MessageKind messageKind,
                String message,
                SourceLocation adaErrorLocation,
                SourceLocation lkqlErrorLocation,
                String ruleName) {

            var adaLoc = adaErrorLocation != null ? adaErrorLocation.display() + ": " : "";
            var lkqlLoc = lkqlErrorLocation != null ? lkqlErrorLocation.display() + ": " : "";
            var rulePart = ruleName.equals("") ? "" : "[" + ruleName.toLowerCase() + "]";

            String sourceString =
                    lkqlErrorLocation != null
                            ? "\n"
                                    + StringUtils.underlineSource(
                                            lkqlErrorLocation, StringUtils.ANSI_YELLOW)
                            : "";

            return adaLoc
                    + lkqlLoc
                    + kindtoString(messageKind)
                    + ": "
                    + message
                    + rulePart
                    + sourceString;
        }
    }

    /** Emitter that formats diagnostics such that the GNATcheck driver can parse them. */
    public static class GNATcheckEmitter implements DiagnosticEmitter {
        @Override
        @CompilerDirectives.TruffleBoundary
        public void emitRuleViolation(
                BaseChecker checker,
                String message,
                SourceLocation violationLocation,
                Libadalang.AdaNode[] genericInstantiations,
                LKQLContext context) {
            // Append generic instantiation information to the message
            if (genericInstantiations.length > 0) {
                StringBuilder messageBuilder = new StringBuilder(message);
                for (int i = 0; i < genericInstantiations.length; ++i) {
                    if (i > 0) {
                        messageBuilder.append(" [");
                    } else {
                        messageBuilder.append(" [instance at ");
                    }
                    final Libadalang.AdaNode inst = genericInstantiations[i];
                    messageBuilder.append(FileUtils.baseName(inst.getUnit().getFileName()));
                    messageBuilder.append(":");
                    messageBuilder.append(inst.getSourceLocationRange().start.line);
                }
                messageBuilder.append("]".repeat(genericInstantiations.length));
                message = messageBuilder.toString();
            }

            // Print the things
            context.println(
                    violationLocation.display()
                            + ": "
                            + "check: "
                            + message
                            + " ["
                            + (checker.getAlias() == null ? "" : checker.getAlias() + "|")
                            + StringUtils.toLowerCase(checker.getName())
                            + "]");
        }

        @Override
        public String diagnostic(
                MessageKind messageKind,
                String message,
                SourceLocation adaErrorLocation,
                SourceLocation lkqlErrorLocation,
                String ruleName) {

            var adaLoc = adaErrorLocation != null ? adaErrorLocation.display(true) + ": " : "";
            var lkqlLoc =
                    lkqlErrorLocation != null
                            ? "internal error at " + lkqlErrorLocation.display(true) + ": "
                            : "";
            var rulePart = ruleName.equals("") ? "" : " [" + ruleName.toLowerCase() + "]";

            return adaLoc + kindtoString(messageKind) + ": " + lkqlLoc + message + rulePart;
        }

        @Override
        public boolean useFullFilePath() {
            return true;
        }

        @Override
        public String kindtoString(MessageKind messageKind) {
            return switch (messageKind) {
                case WARNING -> "warning";
                case ERROR -> "error";
                case RULE_VIOLATION -> "check";
            };
        }

        @Override
        public void emitFileNotFound(SourceLocation from, String fileName, boolean isError) {

            this.emitDiagnostic(
                    isError ? MessageKind.ERROR : MessageKind.WARNING,
                    "internal error: File "
                            + (useFullFilePath() ? fileName : FileUtils.baseName(fileName))
                            + " not found",
                    from,
                    null,
                    "");
        }
    }
}
