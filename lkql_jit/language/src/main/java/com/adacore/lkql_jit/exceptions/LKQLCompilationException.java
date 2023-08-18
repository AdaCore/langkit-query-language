package com.adacore.lkql_jit.exceptions;

import com.adacore.lkql_jit.utils.SourceLocation;
import com.oracle.truffle.api.CompilerDirectives;


/**
 * This exception represents an error in the static analysis of the LKQL code.
 *
 * @author Hugo GUERRIER
 */
public final class LKQLCompilationException extends BaseLKQLException {

    // ----- Attributes -----

    /**
     * Message of the exception.
     */
    private final String message;

    /**
     * Location of the exception.
     */
    private final SourceLocation location;

    // ----- Constructors -----

    /**
     * Create a new LKQL compilation exception with message and location.
     *
     * @param message  The exception message.
     * @param location The exception location.
     */
    private LKQLCompilationException(
        final String message,
        final SourceLocation location
    ) {
        super(formatFull(message, location));
        this.message = message;
        this.location = location;
    }

    // ----- Exception creation methods -----

    // --- Misc exceptions

    /**
     * Create a compilation exception from a simple message.
     *
     * @param message The message of the exception.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException fromMessage(final String message) {
        return new LKQLCompilationException(message, null);
    }

    /**
     * Create a compilation exception from a message and a location.
     *
     * @param message  The message of the exception.
     * @param location The location of the exception.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException fromMessage(
        final String message,
        final SourceLocation location
    ) {
        return new LKQLCompilationException(message, location);
    }

    // --- Symbol exceptions

    /**
     * Create a new compilation exception for an unknown symbol access.
     *
     * @param symbol   The symbol that we're trying to access.
     * @param location The location of the unknown symbol.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException unknownSymbol(
        final String symbol,
        final SourceLocation location
    ) {
        return new LKQLCompilationException("Unknown symbol: " + symbol, location);
    }

    /**
     * Create a new compilation exception for an already existing symbol.
     *
     * @param symbol   The symbol that already exists.
     * @param location The location of the erroneous symbol.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException existingSymbol(
        final String symbol,
        final SourceLocation location
    ) {
        return new LKQLCompilationException("Already existing symbol: " + symbol, location);
    }

    /**
     * Create a new compilation exception for an already existing parameter.
     *
     * @param symbol   The symbol that already exists in the parameters.
     * @param location The node that try to create an already existing parameter.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException existingParameter(
        final String symbol,
        final SourceLocation location
    ) {
        return new LKQLCompilationException("Already existing parameter: " + symbol, location);
    }

    // --- Import exceptions

    /**
     * Create a new compilation exception for a wrong import statement, if the module is not found.
     *
     * @param moduleName The name of the module which isn't found.
     * @param location   The wrong import location.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException moduleNotFound(
        final String moduleName,
        final SourceLocation location
    ) {
        return new LKQLCompilationException("Cannot import, module not found \"" + moduleName + "\"", location);
    }

    // --- Pattern exceptions

    /**
     * Create a new compilation exception for an invalid node kind name.
     *
     * @param location The location of the invalid kind name.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException invalidKindName(final SourceLocation location) {
        return new LKQLCompilationException("Invalid kind name", location);
    }

    /**
     * Create a new compilation exception when a regex is invalid.
     *
     * @param regex    The invalid regex.
     * @param location The location of the pattern compilation.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException regexSyntaxError(
        final String regex,
        final SourceLocation location
    ) {
        return new LKQLCompilationException("Failed to compile regular expression: " + regex, location);
    }

    // --- Call exceptions

    /**
     * Create a new compilation exception for multiple argument with the same name in a function call.
     *
     * @param location The location of the same name argument.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException multipleSameNameArgument(
        final SourceLocation location
    ) {
        return new LKQLCompilationException("Multiple arguments with the same name", location);
    }

    /**
     * Create a new compilation exception for a positional argument after a named argument.
     *
     * @param location The location of the wrong positional argument.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLCompilationException positionAfterNamedArgument(
        final SourceLocation location
    ) {
        return new LKQLCompilationException("positional argument after named argument", location);
    }

}
