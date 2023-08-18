package com.adacore.lkql_jit.exceptions;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.utils.SourceLocation;
import com.oracle.truffle.api.source.Source;

import java.util.List;


/**
 * This exception represents an error in the LKQL source code parsing. It is built from one or more Langkit diagnostics.
 *
 * @author Hugo GUERRIER
 */
public final class LKQLParsingException extends BaseLKQLException {

    // ----- Attributes -----

    /**
     * List of parsing diagnostics.
     */
    private final List<Liblkqllang.Diagnostic> diagnostics;

    /**
     * Truffle source which the parsing diagnostics belongs to.
     */
    private final Source source;

    // ----- Constructors -----

    /**
     * Create a new LKQL parsing exception from the Langkit diagnostics.
     *
     * @param diagnostics The Langkit diagnostics.
     * @param source      The Truffle source representing the parsed LKQL source.
     */
    public LKQLParsingException(
        final List<Liblkqllang.Diagnostic> diagnostics,
        final Source source
    ) {
        super(formatDiagnostics(diagnostics, source));
        this.diagnostics = diagnostics;
        this.source = source;
    }

    // ----- Class methods -----

    /**
     * Get the concatenated representation of given diagnostics
     *
     * @param diagnostics The diagnostics to display.
     * @param source      The Truffle source where the diagnostics belong.
     * @return The diagnostics represented in a string.
     */
    private static String formatDiagnostics(
        final List<Liblkqllang.Diagnostic> diagnostics,
        final Source source
    ) {
        StringBuilder res = new StringBuilder();
        for (Liblkqllang.Diagnostic diagnostic : diagnostics) {
            final SourceLocation location = new SourceLocation(source, diagnostic.sourceLocationRange);
            res.append(formatFull(diagnostic.message.toString(), location))
                .append(System.lineSeparator());
        }
        return res.toString();
    }

}
