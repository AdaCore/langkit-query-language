/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.exceptions;

import com.adacore.lkql_jit.utils.SourceLocation;
import com.oracle.truffle.api.CompilerDirectives;


/**
 * This exception means that there is an error in the LKQL execution.
 *
 * @author Hugo GUERRIER
 */
public final class LKQLRuntimeException extends BaseLKQLException {

    // ----- Attributes -----

    /**
     * The message of the exception.
     */
    private final String message;

    /**
     * The location of the exception.
     */
    private final SourceLocation location;

    // ----- Constructors -----

    /**
     * Create a new LKQL runtime exception with its message a location.
     *
     * @param message  The message of the exception.
     * @param location The location of the exception.
     */
    private LKQLRuntimeException(
        final String message,
        final SourceLocation location
    ) {
        super(formatFull(message, location));
        this.message = message;
        this.location = location;
    }

    // ----- Getters -----

    public SourceLocation getSourceLocation() {
        return location;
    }

    public String getSourceMessage() {
        return message;
    }

    // ----- Exception creation methods -----

    // --- Misc exceptions

    /**
     * Create an exception from a message without any location.
     *
     * @param message The message of the exception.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(final String message) {
        return new LKQLRuntimeException(message, null);
    }

    /**
     * Create a new exception with an arbitrary message and a location.
     *
     * @param message  The message of the exception.
     * @param location The location where there is an error.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(
        final String message,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException(message, location);
    }

    /**
     * Create an exception from a Java exception with the location.
     *
     * @param e        The Java exception.
     * @param location The location of the LKQL call.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromJavaException(
        final Throwable e,
        final SourceLocation location
    ) {
        LKQLRuntimeException res = new LKQLRuntimeException(
            "Error from Java: " + e.getMessage(),
            location
        );
        res.setStackTrace(e.getStackTrace());
        return res;
    }

    /**
     * Create a new exception for the node that shouldn't execute.
     *
     * @param location The location of the node that shouldn't execute.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException shouldNotExecute(final SourceLocation location) {
        return new LKQLRuntimeException("Should not execute", location);
    }

    /**
     * Create an exception when an expression result is ignored in a block expression.
     *
     * @param location The location of the error.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException ignoredExpressionReturn(final SourceLocation location) {
        return new LKQLRuntimeException("Can't ignore the return value of an expr in a block expr", location);
    }

    // --- Symbol exceptions

    /**
     * Create a new exception for a symbol usage which occurs before its assignment.
     *
     * @param symbol   The symbol that we're trying to access.
     * @param location The location of the symbol usage.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException usedBeforeAssignment(
        final String symbol,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Using symbol before its assignment: " + symbol, location);
    }

    // --- Typing exceptions

    /**
     * Create an exception for a wrong type error.
     *
     * @param expected The expected type.
     * @param actual   The actual type.
     * @param location The location of the exception.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongType(
        final String expected,
        final String actual,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Type error: expected " + expected + " but got " + actual, location);
    }

    /**
     * Create an exception when the conversion is not possible between LKQL and Java bindings.
     *
     * @param source   The source LKQL type.
     * @param target   The target Java type.
     * @param location The location of the conversion.
     * @return The newly created exception.
     */
    public static LKQLRuntimeException conversionError(
        final String source,
        final String target,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Cannot convert a " + source + " to a " + target, location);
    }

    /**
     * Create an exception for a wrong from clause.
     *
     * @param location The location of the invalid from clause.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongFrom(final SourceLocation location) {
        return new LKQLRuntimeException("Wrong kind of element in `from clause`", location);
    }

    /**
     * Create an exception when an invalid list is used in a from clause.
     *
     * @param location The location of the invalid from clause.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongFromList(final SourceLocation location) {
        return new LKQLRuntimeException("Wrong kind of element in list for `from clause`", location);
    }

    /**
     * Create an exception when a selector arm returns an invalid type.
     *
     * @param wrongType The invalid returned type.
     * @param location  The location of the returned expression.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongSelectorType(
        final String wrongType,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Cannot use values of kind " + wrongType + " in a selector", location);
    }

    /**
     * Create an exception for a wrong pattern type.
     *
     * @param wrongType The wrong pattern type in a string.
     * @param location  The location of the exception.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongPatternType(
        final String wrongType,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Invalid pattern kind: " + wrongType, location);
    }

    /**
     * Create an exception when an unsupported type is fetched from Libadalang.
     *
     * @param type     The name of the type that is not supported.
     * @param location The location of type conversion try.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unsupportedType(
        final String type,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Unsupported value type from the introspection API: " + type, location);
    }

    // --- Operator exception

    /**
     * Create an exception for an unsupported operation.
     *
     * @param leftType  The left operand type.
     * @param op        The operator.
     * @param rightType The right operand type.
     * @param location  The location of the operation.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unsupportedOperation(
        final String leftType,
        final String op,
        final String rightType,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Unsupported operation: " + leftType + " " + op + " " + rightType, location);
    }

    /**
     * Create an exception when user tries to divide by zero.
     *
     * @param location The location of the operation which tries to divide by zero.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException divByZero(
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Zero division", location);
    }

    /**
     * Create an exception for an invalid indexing operation.
     *
     * @param index    The invalid index.
     * @param location The location of the wrong indexing operation.
     * @return The newly created exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException invalidIndex(
        final int index,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Invalid index: " + index, location);
    }

    /**
     * Create an exception when the user tries to access to a non-existing member.
     *
     * @param location The location of the member accessing try.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException noSuchMember(final SourceLocation location) {
        return new LKQLRuntimeException("No such member", location);
    }

    /**
     * Create an exception when the user tries to access to a non-existing field.
     *
     * @param location The location of the field accessing try.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException noSuchField(
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("No such field", location);
    }

    /**
     * Create an exception when a member access is done on the wrong type.
     *
     * @param member   The wrong member to access.
     * @param type     The accessed type.
     * @param location The location of the wrong member access.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongMember(
        final String member,
        final String type,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Cannot get member " + member + " for " + type + " value", location);
    }

    /**
     * Create an exception for a member access on a null value.
     *
     * @param location The location of the access on null.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException nullReceiver(
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Null receiver in dot access", location);
    }

    // --- Argument exception

    /**
     * Create an exception for a wrong arity function call.
     *
     * @param expected The expected arity.
     * @param actual   The actual arity.
     * @param location The location of the wrong call.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongArity(
        final int expected,
        final int actual,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Expected " + expected + " arguments but got " + actual, location);
    }

    /**
     * Create an exception for an unknown argument in a function call.
     *
     * @param unknown  The unknown argument name.
     * @param location The location of the unknown argument.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unknownArgument(
        final String unknown,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Unknown argument name: " + unknown, location);
    }

    /**
     * Create an exception for a missing argument in a function call.
     *
     * @param missingIndex The missing argument index.
     * @param location     The location of the wrong call.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException missingArgument(
        final int missingIndex,
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Missing value for param # " + missingIndex + " in call", location);
    }

    /**
     * Create an exception for a selector call without any node.
     *
     * @param location The location of the selector call.
     * @return The newly created exception.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException selectorWithoutNode(
        final SourceLocation location
    ) {
        return new LKQLRuntimeException("Selector call should have a node argument", location);
    }

}
