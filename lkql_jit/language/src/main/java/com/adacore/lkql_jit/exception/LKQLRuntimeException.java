//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exception;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
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

    private LKQLRuntimeException(String message, Node location) {
        super(message, location);
    }

    // ----- Exception creation methods -----

    // --- Misc exception

    /** Create a new exception with an arbitrary message and a location. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(String message, Node location) {
        return new LKQLRuntimeException(message, location);
    }

    /**
     * Create an exception from a message without any location.
     *
     * @param message The message of the exception.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(String message) {
        return new LKQLRuntimeException(message, null);
    }

    /**
     * Create an exception from a Java exception with the location.
     *
     * @param e The Java exception.
     * @param location The location of the LKQL call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromJavaException(Throwable e, Node location) {
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
    public static LKQLRuntimeException shouldNotExecute(Node location) {
        return LKQLRuntimeException.fromMessage("Should not execute", location);
    }

    /**
     * Create an exception which should not happened during a normal execution.
     *
     * @param message Additional message.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException shouldNotHappen(String message) {
        return new LKQLRuntimeException("This exception should not happen: " + message, null);
    }

    /**
     * Create an exception when a regex has a wrong syntax.
     *
     * @param regex The regex.
     * @param location The location of the pattern compilation.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException regexSyntaxError(String regex, Node location) {
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
    public static LKQLRuntimeException ignoredExpressionReturn(Node location) {
        return LKQLRuntimeException.fromMessage(
                "Can't ignore the return value of an expr in a block expr", location);
    }

    /** Create a new exception when a key appears many times in the same object literal. */
    public static LKQLRuntimeException multipleSameNameKeys(String key, Node location) {
        return LKQLRuntimeException.fromMessage(
                "Multiple keys with the same name in the object: \"" + key + "\"", location);
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
    public static LKQLRuntimeException unknownSymbol(String symbol, Node location) {
        return LKQLRuntimeException.fromMessage("Unknown symbol: " + symbol, location);
    }

    /**
     * Create a new exception for an invalid node kind name request.
     *
     * @param location The location of the request.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException invalidKindName(Node location) {
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
    public static LKQLRuntimeException moduleNotFound(String moduleName, Node location) {
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
    public static LKQLRuntimeException wrongType(String expected, String actual, Node location) {
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
            String source, String target, Node location) {
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
    public static LKQLRuntimeException wrongFrom(Node location) {
        return LKQLRuntimeException.fromMessage("Wrong kind of element in `from clause`", location);
    }

    /**
     * Create an exception for a wrong list in a from clause.
     *
     * @param location The location of the from clause.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongFromList(Node location) {
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
    public static LKQLRuntimeException unsupportedType(String type, Node location) {
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
            String leftType, String op, String rightType, Node location) {
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
    public static LKQLRuntimeException divByZero(Node location) {
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
    public static LKQLRuntimeException invalidIndex(int index, Node location) {
        return LKQLRuntimeException.fromMessage("Invalid index: " + index, location);
    }

    /**
     * Create an exception for a not such member exception.
     *
     * @param location The node that try to get a wrong member.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException noSuchMember(Node location) {
        return LKQLRuntimeException.fromMessage("No such member", location);
    }

    /**
     * Create an exception for a no such field exception.
     *
     * @param location The location of the exception.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException noSuchField(Node location) {
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
    public static LKQLRuntimeException wrongMember(String member, String type, Node location) {
        return LKQLRuntimeException.fromMessage(
                "Cannot get member `%s` for %s value".formatted(member, type), location);
    }

    /**
     * Create an exception for a member access on a null value.
     *
     * @param location The node which produce the exception.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException nullReceiver(Node location) {
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
    public static LKQLRuntimeException wrongArity(int expected, int actual, Node location) {
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
    public static LKQLRuntimeException unknownArgument(String unknown, Node location) {
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
    public static LKQLRuntimeException missingArgument(int missingIndex, Node location) {
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
    public static LKQLRuntimeException selectorWithoutNode(Node location) {
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
    public static LKQLRuntimeException multipleSameNameArgument(Node location) {
        return LKQLRuntimeException.fromMessage("Multiple arguments with the same name", location);
    }

    /**
     * Create an exception for a positional argument after a named argument.
     *
     * @param location The node that did the wrong call.
     * @return The exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException positionAfterNamedArgument(Node location) {
        return LKQLRuntimeException.fromMessage(
                "positional argument after named argument", location);
    }

    public SourceLocation getSourceLoc() {
        var loc = getLocation();
        return loc != null ? new SourceSectionWrapper(getLocation().getSourceSection()) : null;
    }

    /**
     * Get the raw message for the diagnostic associated with this exception.
     *
     * @return The raw message without filename and source listing.
     */
    @CompilerDirectives.TruffleBoundary
    public String getErrorMessage() {
        return super.getMessage();
    }

    /**
     * Get the message for this exception, formatted as a diagnostic. This is necessary because when
     * catching LKQL runtime exceptions from the outside of Truffle, we still want to be able to
     * format them correctly, even though we know nothing about LKQLRuntimeException, which is an
     * internal exception from there.
     */
    @CompilerDirectives.TruffleBoundary
    @Override
    public String getMessage() {
        var loc = getSourceLoc();
        return LKQLLanguage.getContext(null)
                .getDiagnosticEmitter()
                .diagnostic(CheckerUtils.MessageKind.ERROR, this.getErrorMessage(), null, loc, "");
    }
}
