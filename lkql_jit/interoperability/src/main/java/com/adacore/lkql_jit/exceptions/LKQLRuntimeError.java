//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exceptions;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import java.io.Serial;
import java.util.Stack;
import java.util.stream.Collectors;

/**
 * This exception class represents all errors that may happen during the execution of some LKQL
 * source. An instance of this exception represents an error in a LKQL source, not a bug in the LKQL
 * engine.
 */
public final class LKQLRuntimeError extends AbstractTruffleException {

    // ----- Attributes -----

    @Serial
    private static final long serialVersionUID = 8401390548003855662L;

    // ----- Constructors -----

    private LKQLRuntimeError(String message, Node location, Throwable cause) {
        super(message, cause, -1, location);
    }

    /** Create a new exception with an arbitrary message and a location. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError create(String message, Node location) {
        return new LKQLRuntimeError(message, location, null);
    }

    // ----- Exception creation methods -----

    // --- Misc exception

    /** Create an exception when an error occurs in the Langkit analysis library. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError langkitError(Throwable cause, Node location) {
        return new LKQLRuntimeError(cause.getMessage(), location, cause);
    }

    /** Create an exception when a regex has a wrong syntax. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError regexSyntaxError(String regex, Node location) {
        return LKQLRuntimeError.create(regexSyntaxErrorMessage(regex), location);
    }

    /** Create an exception when an expression result is ignored in a block expression. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError ignoredExpressionReturn(Node location) {
        return LKQLRuntimeError.create(
            "Can't ignore the return value of an expr in a block expr",
            location
        );
    }

    /** Create an exception when an empty stream's head is accessed. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError emptyStreamHead(Node location) {
        return LKQLRuntimeError.create("Can't get head from an empty stream", location);
    }

    /** Create an exception when an empty stream's tail is accessed. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError emptyStreamTail(Node location) {
        return LKQLRuntimeError.create("Can't get tail from an empty stream", location);
    }

    // --- Symbol exception

    /** Create a new exception for an unknown symbol access. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError unknownSymbol(String symbol, Node location) {
        return LKQLRuntimeError.create(unknownSymbolMessage(symbol), location);
    }

    // --- Importation related exceptions

    /** Create a new exception when there is a circular dependency. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError circularDependency(
        Stack<Source> importStack,
        Source responsible,
        Node location
    ) {
        return LKQLRuntimeError.create(
            "Circular dependency in LKQL modules (" +
                importStack.stream().map(Source::getName).collect(Collectors.joining(" -> ")) +
                " -> " +
                responsible.getName() +
                ")",
            location
        );
    }

    // --- Typing exception

    /** Create an exception for a wrong type error. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError wrongType(String expected, String actual, Node location) {
        return LKQLRuntimeError.create(
            "Type error: expected " + expected + " but got " + actual,
            location
        );
    }

    /** Create an exception when the conversion is not possible between LKQL and Java bindings. */
    public static LKQLRuntimeError conversionError(String source, String target, Node location) {
        return LKQLRuntimeError.create("Cannot convert a " + source + " to a " + target, location);
    }

    /** Create an exception for a wrong from clause. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError wrongFrom(Node location) {
        return LKQLRuntimeError.create("Wrong kind of element in `from clause`", location);
    }

    /** Create an exception for a wrong list in a from clause. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError wrongFromList(Node location) {
        return LKQLRuntimeError.create("Wrong kind of element in list for `from clause`", location);
    }

    /** Create an exception for an unsupported type. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError unsupportedType(Class<?> type, Node location) {
        return LKQLRuntimeError.create(
            "Unsupported value type from the introspection API: " + type.getSimpleName(),
            location
        );
    }

    // --- Operator exception

    /** Create an exception for an unsupported operation. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError unsupportedOperation(
        String leftType,
        String op,
        String rightType,
        Node location
    ) {
        return LKQLRuntimeError.create(
            "Unsupported operation: " + leftType + " " + op + " " + rightType,
            location
        );
    }

    /** Create an exception for a division by zero. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError divByZero(Node location) {
        return LKQLRuntimeError.create("Zero division", location);
    }

    /** Create an exception for an invalid index operation. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError invalidIndex(int index, Node location) {
        return LKQLRuntimeError.create("Invalid index: " + index, location);
    }

    /** Create an exception for a not such member exception. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError noSuchMember(Node location) {
        return LKQLRuntimeError.create("No such member", location);
    }

    /** Create an exception for a no such field exception. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError noSuchField(Node location) {
        return LKQLRuntimeError.create(noSuchFieldMessage(), location);
    }

    /** Create an exception for a wrong member access on the wrong type. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError wrongMember(String member, String type, Node location) {
        return LKQLRuntimeError.create(
            "Cannot get member `%s` for %s value".formatted(member, type),
            location
        );
    }

    /** Create an exception for a member access on a null value. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError nullReceiver(Node location) {
        return LKQLRuntimeError.create("Null receiver in dot access", location);
    }

    /** Create an exception when there is a collision during an object combination. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError objectCombiningCollision(String collidingMember, Node location) {
        return LKQLRuntimeError.create(
            "Cannot combine objects, both define the \"" + collidingMember + "\" member",
            location
        );
    }

    // --- Argument exception

    /** Create an exception for a wrong arity function call. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError wrongArity(int expected, int actual, Node location) {
        return LKQLRuntimeError.create(wrongArityMessage(expected, actual), location);
    }

    /** Create an exception for an unknown argument in a function call. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError unknownArgument(String unknown, Node location) {
        return LKQLRuntimeError.create(unknownArgumentMessage(unknown), location);
    }

    /** Create an exception for a missing argument in a function call. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError missingArgument(int missingIndex, String missingName) {
        return LKQLRuntimeError.create(
            "Missing value for param #" + missingIndex + " (named \"" + missingName + "\") in call",
            null
        );
    }

    /** Create an exception for a selector call without any node. */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeError selectorWithoutNode(Node location) {
        return LKQLRuntimeError.create("Selector call should have a node argument", location);
    }

    // ----- Message creation methods -----

    /** Get a message to report regex syntax errors. */
    public static String regexSyntaxErrorMessage(String regex) {
        return "Failed to compile regular expression: " + regex;
    }

    /** Get a message to report an unknown symbol. */
    public static String unknownSymbolMessage(String symbol) {
        return "Unknown symbol: " + symbol;
    }

    /** Get a message to report an invalid field access. */
    public static String noSuchFieldMessage() {
        return "No such field";
    }

    /** Get a message to report a call with the wrong argument arity. */
    public static String wrongArityMessage(int expected, int actual) {
        return "Expected " + expected + " arguments but got " + actual;
    }

    /** Get a message to report when an unknown argument name is provided. */
    public static String unknownArgumentMessage(String unknown) {
        return "Unknown argument name: " + unknown;
    }
}
