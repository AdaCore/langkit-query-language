//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exception;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.Locatable;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.source.Source;
import java.io.Serial;

/**
 * This exception means that there is an error in the LKQL execution.
 *
 * @author Hugo GUERRIER
 */
public final class LKQLRuntimeException extends AbstractTruffleException {

    // ----- Attributes -----

    @Serial private static final long serialVersionUID = 8401390548003855662L;

    // ----- Constructors -----

    /**
     * @see AbstractTruffleException#AbstractTruffleException(String)
     */
    private LKQLRuntimeException(String message) {
        super(message);
    }

    // ----- Exception creation methods -----

    // --- Misc exception

    /** Create a new exception with an arbitrary message and a location. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(String message, SourceLocation location) {
        return new LKQLRuntimeException(fullErrorText(message, location));
    }

    /** Create a new exception with an arbitrary message and a location. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(String message, Locatable location) {
        return new LKQLRuntimeException(fullErrorText(message, location));
    }

    /**
     * Create an exception from a message without any location.
     *
     * @param message The message of the exception.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(String message) {
        return new LKQLRuntimeException(message);
    }

    /**
     * Create an exception from a Java exception with the location.
     *
     * @param e The Java exception.
     * @param location The location of the LKQL call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromJavaException(Throwable e, SourceLocation location) {
        LKQLRuntimeException res =
                LKQLRuntimeException.fromMessage(
                        "Error from Java bindings: " + e.getMessage(), location);
        res.setStackTrace(e.getStackTrace());
        return res;
    }

    /**
     * Create an exception from a Java exception with the location.
     *
     * @param e The Java exception.
     * @param location The location of the LKQL call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromJavaException(Throwable e, Locatable location) {
        LKQLRuntimeException res =
                LKQLRuntimeException.fromMessage(
                        "Error from Java bindings: " + e.getMessage(), location);
        res.setStackTrace(e.getStackTrace());
        return res;
    }

    /**
     * Create a new exception for the node that shouldn't execute.
     *
     * @param location The node that shouldn't execute.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException shouldNotExecute(Locatable location) {
        return LKQLRuntimeException.fromMessage("Should not execute", location);
    }

    /**
     * Create an exception which should not happened during a normal execution.
     *
     * @param message Additional message.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException shouldNotHappen(String message) {
        return new LKQLRuntimeException("This exception should not happen: " + message);
    }

    /**
     * Create a new exception for a parsing exception in the LKQL sources.
     *
     * @param diagnostics The diagnostics of the LKQL parsing.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException parsingException(
            Liblkqllang.Diagnostic[] diagnostics, Source source) {
        // Prepare the error message builder
        StringBuilder builder = new StringBuilder();

        // Iterate over diagnostics
        for (Liblkqllang.Diagnostic diagnostic : diagnostics) {
            builder.append(
                    fullErrorText(
                            diagnostic.message.toString(),
                            new DummyLocation(
                                    new SourceLocation(source, diagnostic.sourceLocationRange))));
            builder.append('\n');
        }

        // Return the created exception
        return new LKQLRuntimeException(builder.toString());
    }

    /**
     * Create an exception when a regex has a wrong syntax.
     *
     * @param regex The regex.
     * @param location The location of the pattern compilation.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException regexSyntaxError(String regex, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Failed to compile regular expression: " + regex, location);
    }

    /**
     * Create an exception when an expression result is ignored in a block expression.
     *
     * @param location The location of the error.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException ignoredExpressionReturn(Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Can't ignore the return value of an expr in a block expr", location);
    }

    /** Create a new exception when a key appears many times in the same object literal. */
    public static LKQLRuntimeException multipleSameNameKeys(String key, Locatable location) {
        return new LKQLRuntimeException(
                fullErrorText(
                        "Multiple keys with the same name in the object: \"" + key + "\"",
                        location));
    }

    // --- Symbol exception

    /**
     * Create a new exception for an unknown symbol access.
     *
     * @param symbol The symbol that we're trying to access.
     * @param location The node that access the symbol.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unknownSymbol(String symbol, Locatable location) {
        return LKQLRuntimeException.fromMessage("Unknown symbol: " + symbol, location);
    }

    /**
     * Create a new exception for an already existing symbol.
     *
     * @param symbol The symbol that already exists.
     * @param location The node that try to create an already existing symbol.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException existingSymbol(String symbol, Locatable location) {
        return LKQLRuntimeException.fromMessage("Already existing symbol: " + symbol, location);
    }

    /**
     * Create a new exception for an already existing parameter.
     *
     * @param symbol The symbol that already exists in the parameters.
     * @param location The node that try to create an already existing parameter.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException existingParameter(String symbol, Locatable location) {
        return LKQLRuntimeException.fromMessage("Already existing parameter: " + symbol, location);
    }

    /**
     * Create a new exception for an invalid node kind name request.
     *
     * @param location The location of the request.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException invalidKindName(Locatable location) {
        return LKQLRuntimeException.fromMessage("Invalid kind name", location);
    }

    /**
     * Create a new exception for a wrong import statement, if the module is not found.
     *
     * @param moduleName The name of the module which isn't found.
     * @param location The wrong import node.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException moduleNotFound(String moduleName, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Cannot import, module not found \"" + moduleName + "\"", location);
    }

    // --- Typing exception

    /**
     * Create an exception for a wrong type error.
     *
     * @param expected The expected type.
     * @param actual The actual type.
     * @param location The node which rose the exception.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongType(
            String expected, String actual, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Type error: expected " + expected + " but got " + actual, location);
    }

    /**
     * Create an exception when the conversion is not possible between LKQL and Java bindings.
     *
     * @param source The source LKQL type.
     * @param target The target Java type.
     * @param location The node which tries to do the conversion.
     * @return The exception.
     */
    public static LKQLRuntimeException conversionError(
            String source, String target, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Cannot convert a " + source + " to a " + target, location);
    }

    /**
     * Create an exception for a wrong from clause.
     *
     * @param location The location of the from clause.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongFrom(Locatable location) {
        return LKQLRuntimeException.fromMessage("Wrong kind of element in `from clause`", location);
    }

    /**
     * Create an exception for a wrong list in a from clause.
     *
     * @param location The location of the from clause.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongFromList(Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Wrong kind of element in list for `from clause`", location);
    }

    /**
     * Create an exception for an unsupported type.
     *
     * @param type The type name that is not supported.
     * @param location The location of the error.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unsupportedType(String type, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Unsupported value type from the introspection API: " + type, location);
    }

    // --- Operator exception

    /**
     * Create an exception for an unsupported operation.
     *
     * @param leftType The left operand type.
     * @param op The operator.
     * @param rightType The right operand type.
     * @param location The node that try to do the unsupported operation.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unsupportedOperation(
            String leftType, String op, String rightType, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Unsupported operation: " + leftType + " " + op + " " + rightType, location);
    }

    /**
     * Create an exception for a division by zero.
     *
     * @param location The node which try to divide by zero.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException divByZero(Locatable location) {
        return LKQLRuntimeException.fromMessage("Zero division", location);
    }

    /**
     * Create an exception for an invalid index operation.
     *
     * @param index The invalid index.
     * @param location The node which try to access the wrong index.
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException invalidIndex(int index, Locatable location) {
        return LKQLRuntimeException.fromMessage("Invalid index: " + index, location);
    }

    /**
     * Create an exception for a not such member exception.
     *
     * @param location The node that try to get a wrong member.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException noSuchMember(Locatable location) {
        return LKQLRuntimeException.fromMessage("No such member", location);
    }

    /**
     * Create an exception for a no such field exception.
     *
     * @param location The location of the exception.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException noSuchField(Locatable location) {
        return LKQLRuntimeException.fromMessage("No such field", location);
    }

    /**
     * Create an exception for a wrong member access on the wrong type.
     *
     * @param member The member to access.
     * @param type The accessed type.
     * @param location The node that try to access the wrong member.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongMember(String member, String type, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Cannot get member " + member + " for " + type + " value", location);
    }

    /**
     * Create an exception for a member access on a null value.
     *
     * @param location The node which produce the exception.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException nullReceiver(Locatable location) {
        return LKQLRuntimeException.fromMessage("Null receiver in dot access", location);
    }

    // --- Argument exception

    /**
     * Create an exception for a wrong arity function call.
     *
     * @param expected The expected arity.
     * @param actual The actual arity.
     * @param location The node which did the wrong call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongArity(int expected, int actual, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Expected " + expected + " arguments but got " + actual, location);
    }

    /**
     * Create an exception for an unknown argument in a function call.
     *
     * @param unknown The unknown argument name.
     * @param location The node that call the unknown argument.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unknownArgument(String unknown, Locatable location) {
        return LKQLRuntimeException.fromMessage("Unknown argument name: " + unknown, location);
    }

    /**
     * Create an exception for a missing argument in a function call.
     *
     * @param missingIndex The missing argument index.
     * @param location The node that did the wrong call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException missingArgument(int missingIndex, Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Missing value for param # " + missingIndex + " in call", location);
    }

    /**
     * Create an exception for a selector call without any node.
     *
     * @param location The location of the selector call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException selectorWithoutNode(Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "Selector call should have a node argument", location);
    }

    /**
     * Create an exception for multiple argument with the same name.
     *
     * @param location The node that did the multiple same name call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException multipleSameNameArgument(Locatable location) {
        return LKQLRuntimeException.fromMessage("Multiple arguments with the same name", location);
    }

    /**
     * Create an exception for a positional argument after a named argument.
     *
     * @param location The node that did the wrong call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException positionAfterNamedArgument(Locatable location) {
        return LKQLRuntimeException.fromMessage(
                "positional argument after named argument", location);
    }

    // ----- Class methods -----

    /**
     * Get the base exception text : "filename:start_line:start_col: error:".
     *
     * @param location The location of the error.
     * @return The base exception text for all exception.
     */
    private static String baseErrorText(SourceLocation location) {
        if (LKQLLanguage.SUPPORT_COLOR) {
            return StringUtils.ANSI_BOLD
                    + location.toString()
                    + ":"
                    + StringUtils.ANSI_RED
                    + " error: "
                    + StringUtils.ANSI_RESET;
        } else {
            return location.toString() + ": error: ";
        }
    }

    /**
     * Get a string representing the LKQL source with the underlined location.
     *
     * @param location The location to generate source representation for.
     * @return The string representing the LKQL source.
     */
    private static String sourceText(SourceLocation location) {
        // Get the positional information
        int startLine = location.getStartLine();
        int endLine = location.getEndLine();
        int startCol = location.getStartColumn();
        int endCol = location.getEndColumn();

        String[] lines = location.getLines(startLine - 1, endLine);
        return StringUtils.underlineSource(
                lines, startLine, startCol, endLine, endCol, StringUtils.ANSI_RED);
    }

    /**
     * Wrap an error message with the full error text (base text + position text).
     *
     * @param errorMessage The error message.
     * @param location The location where there was an exception.
     * @return The full error text.
     */
    private static String fullErrorText(String errorMessage, Locatable location) {
        return baseErrorText(location.getLocation())
                + errorMessage
                + "\n"
                + sourceText(location.getLocation())
                + "\n";
    }

    private static String fullErrorText(String errorMessage, SourceLocation location) {
        return baseErrorText(location) + errorMessage + "\n" + sourceText(location) + "\n";
    }

    // ----- Instance methods -----

    /**
     * Get the raw message of the exception; just the exception message without the LKQL file name,
     * location and source listing. TODO: Remove this hackish method when
     * https://gitlab.adacore-it.com/eng/libadalang/langkit-query-language/-/merge_requests/89 is
     * merged
     *
     * @return The raw message without filename and source listing.
     */
    @CompilerDirectives.TruffleBoundary
    public String getRawMessage() {
        final String[] splitHeader = this.getMessage().split("\n")[0].split(":");
        return splitHeader[splitHeader.length - 1].strip();
    }

    /**
     * Get the source location of the exception as a string: "my_file.lkql:15:13". TODO: Remove this
     * hackish method when
     * https://gitlab.adacore-it.com/eng/libadalang/langkit-query-language/-/merge_requests/89 is
     * merged
     *
     * @return The error source location as a string.
     */
    @CompilerDirectives.TruffleBoundary
    public String getLocationString() {
        final String[] splitHeader = this.getMessage().split("\n")[0].split(" ");
        return splitHeader[0].substring(0, splitHeader[0].length() - 1);
    }
}
