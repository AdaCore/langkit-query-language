package com.adacore.lkql_jit.exceptions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.SourceLocation;
import com.oracle.truffle.api.exception.AbstractTruffleException;

/**
 * This exception is the base of all LKQL source code related exception.
 *
 * @author Hugo GUERRIER
 */
public abstract class BaseLKQLException extends AbstractTruffleException {

    // ----- Constructors -----

    /**
     * Create a new LKQL exception with its message.
     *
     * @param message The message of the exception.
     */
    protected BaseLKQLException(final String message) {
        super(message);
    }

    // ----- Class methods -----

    /**
     * Get the full format of the exception message from the source message and the source location:
     * <code>
     *     my_file.lkql:1:5: error: My error message
     *     1 | val lkql_source = "Coucou"
     *       |     ^^^^^^^^^^^
     * </code>
     *
     * @param message  The source message of the exception
     * @param location The location of the exception in the LKQL code. This can be null if the exception cannot be
     *                 located.
     * @return The fully formatted exception message.
     */
    protected static String formatFull(
        final String message,
        final SourceLocation location
    ) {
        if (location != null) {
            return locationInFile(location) + message + System.lineSeparator() +
                location.underlinedSource(StringUtils.ANSI_RED) + System.lineSeparator();
        } else {
            return "Error: " + message;
        }
    }

    /**
     * Get the file and location of an LKQL error as a string:
     * <code>
     *     my_file.lkql:1:5: error:
     * </code>
     *
     * @param location The source location to represent.
     * @return The location representation.
     */
    protected static String locationInFile(final SourceLocation location) {
        if (LKQLLanguage.SUPPORT_COLOR) {
            return StringUtils.ANSI_BOLD + location.getFileName() +
                ":" + location.getStartLine() +
                ":" + location.getStartColumn() +
                ":" + StringUtils.ANSI_RED + ": error: " + StringUtils.ANSI_RESET;
        } else {
            return location.getFileName() +
                ":" + location.getStartLine() +
                ":" + location.getStartColumn() +
                ": error: ";
        }
    }

}
