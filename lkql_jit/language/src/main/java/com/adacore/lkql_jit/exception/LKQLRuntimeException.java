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

package com.adacore.lkql_jit.exception;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.Locatable;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.source.Source;
import com.adacore.libadalang.Libadalang;
import com.adacore.liblkqllang.Liblkqllang;

import java.util.List;


/**
 * This exception means that there is an error in the LKQL execution
 *
 * @author Hugo GUERRIER
 */
public final class LKQLRuntimeException extends AbstractTruffleException {

    // ----- Constructors -----

    /** @see AbstractTruffleException#AbstractTruffleException(String) */
    private LKQLRuntimeException(String message) {super(message);}

    // ----- Exception creation methods -----

    // --- Misc exception

    /**
     * Create a new exception with an arbitrary message and a location
     *
     * @param message The message of the exception
     * @param location The node where there is an error
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(String message, Locatable location) {
        return new LKQLRuntimeException(fullErrorText(message, location));
    }

    /**
     * Create an exception from a message without any location
     *
     * @param message The message of the exception
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException fromMessage(String message) {
        return new LKQLRuntimeException("Error: " + message);
    }

    /**
     * Create a new exception for the node that shouldn't execute
     *
     * @param location The node that shouldn't execute
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException shouldNotExecute(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Should not execute", location));
    }

    /**
     * Create a new exception for a parsing exception in the LKQL sources
     *
     * @param diagnostics The diagnostics of the LKQL parsing
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException parsingException(List<Liblkqllang.Diagnostic> diagnostics, Source source) {
        // Prepare the error message builder
        StringBuilder builder = new StringBuilder();

        // Iterate over diagnostics
        for(Liblkqllang.Diagnostic diagnostic : diagnostics) {
            builder.append(fullErrorText(
                    diagnostic.message.toString(),
                    new DummyLocation(new SourceLocation(source, diagnostic.sourceLocationRange)))
            );
            builder.append('\n');
        }

        // Return the created exception
        return new LKQLRuntimeException(builder.toString());
    }

    /**
     * Create an exception when a regex has a wrong syntax
     *
     * @param regex The regex
     * @param location The location of the pattern compilation
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException regexSyntaxError(String regex, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Failed to compile regular expression: " + regex, location));
    }

    /**
     * Create an exception when an expression result is ignored in a block expression
     *
     * @param location The location of the error
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException ignoredExpressionReturn(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Can't ignore the return value of an expr in a block expr", location));
    }

    // --- Symbol exception

    /**
     * Create a new exception for an unknown symbol access
     *
     * @param symbol The symbol that we're trying to access
     * @param location The node that access the symbol
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unknownSymbol(String symbol, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Unknown symbol: " + symbol, location));
    }

    /**
     * Create a new exception for an already existing symbol
     *
     * @param symbol The symbol that already exists
     * @param location The node that try to create an already existing symbol
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException existingSymbol(String symbol, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Already existing symbol: " + symbol, location));
    }

    /**
     * Create a new exception for an invalid node kind name request
     *
     * @param location The location of the request
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException invalidKindName(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Invalid kind name", location));
    }

    /**
     * Create a new exception for a wrong import statement, if the module is not found
     *
     * @param moduleName The module not found
     * @param location The wrong import node
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException moduleNotFound(String moduleName, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Cannot import, module not found \"" + moduleName + "\"", location));
    }

    // --- Typing exception

    /**
     * Create an exception for a wrong type error
     *
     * @param expected The expected type
     * @param actual The actual type
     * @param location The node which rose the exception
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongType(String expected, String actual, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Type error: expected " + expected + " but got " + actual, location));
    }

    /**
     * Create an exception for a wrong from clause
     *
     * @param location The location of the from clause
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongFrom(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Wrong kind of element in `from clause`", location));
    }

    /**
     * Create an exception for a wrong list in a from clause
     *
     * @param location The location of the from clause
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongFromList(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Wrong kind of element in list for `from clause`", location));
    }

    /**
     * Create an exception from a wrong use of type in a selector
     *
     * @param wrongType The used type in the selector
     * @param location The location of the type
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongSelectorType(String wrongType, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Cannot use values of kind " + wrongType + " in a selector", location));
    }

    /**
     * Create an exception for a wrong pattern type
     *
     * @param wrongType The wrong pattern type in a string
     * @param location The location of the exception
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongPatternType(String wrongType, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Invalid pattern kind: " + wrongType, location));
    }

    /**
     * Create an exception for an unsupported type
     *
     * @param type The type name that is not supported
     * @param location The location of the error
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unsupportedType(String type, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Unsupported value type from the introspection API: " + type, location));
    }

    // --- Operator exception

    /**
     * Create an exception for an unsupported operation
     *
     * @param leftType The left type
     * @param op The operator
     * @param rightType The right type
     * @param location The node that try to do the unsupported operation
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unsupportedOperation(String leftType, String op, String rightType, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Unsupported operation: " + leftType + " " + op + " " + rightType, location));
    }

    /**
     * Create an exception for a division by zero
     *
     * @param location The node that try to divide by zero
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException divByZero(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Zero division", location));
    }

    /**
     * Create an exception for an invalid index operation
     *
     * @param index The invalid index
     * @param location The node which try to access the wrong index
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException invalidIndex(int index, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Invalid index: " + index, location));
    }

    /**
     * Create an exception for a not such member exception
     *
     * @param location The node that try to get the wrong memeber
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException noSuchMember(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("No such member", location));
    }

    /**
     * Create an exception for a no such field exception
     *
     * @param member The field you tried to get
     * @param receiver The receiver node
     * @param location The location of the exception
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException noSuchField(String member, Libadalang.AdaNode receiver, Locatable location) {
        return new LKQLRuntimeException(fullErrorText(
                "No field named " + member + " on nodes of kind: " + receiver.getKindName(),
                location
        ));
    }

    /**
     * Create an exception for a wrong member access on the wrong type
     *
     * @param member The member to access
     * @param type The accessed type
     * @param location The node that try to access the wrong member
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongMember(String member, String type, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Cannot get member " + member + " for " + type + " value", location));
    }

    /**
     * Create an exception for a null member access
     *
     * @param location The node that produce the exception
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException nullReceiver(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Null receiver in dot access", location));
    }

    // --- Argument exception

    /**
     * Create an exception for a wrong arity function call
     *
     * @param expected The expected arity
     * @param actual The actual arity
     * @param location The node which did the wrong call
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException wrongArity(int expected, int actual, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Expected " + expected + " arguments but got " + actual, location));
    }

    /**
     * Create an exception for an unknown argument in a function call
     *
     * @param unknown The unknown argument name
     * @param location The node that call the unknown argument
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException unknownArgument(String unknown, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Unknown argument name: " + unknown, location));
    }

    /**
     * Create an exception for a missing argument in a function call
     *
     * @param missingIndex The missing argument index
     * @param location The node that did the wrong call
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException missingArgument(int missingIndex, Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Missing value for param # " + missingIndex + " in call", location));
    }

    /**
     * Create an exception for a selector call without any node
     *
     * @param location The location of the selector call
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException selectorWithoutNode(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Selector call should have a node argument", location));
    }

    /**
     * Create an exception for multiple argument with the same name
     *
     * @param location The node that did the multiple same name call
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException multipleSameNameArgument(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("Multiple arguments with the same name", location));
    }

    /**
     * Create an exception for a positional argument after a named argument
     *
     * @param location The node that did the wrong call
     * @return The exception
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLRuntimeException positionAfterNamedArgument(Locatable location) {
        return new LKQLRuntimeException(fullErrorText("positional argument after named argument", location));
    }

    // ----- Class methods -----

    /**
     * Get the base exception text : "filename:start_line:start_col: error:"
     *
     * @param location The location of the error
     * @return The base exception text for all exception
     */
    private static String baseErrorText(Locatable location) {
        if(LKQLLanguage.SUPPORT_COLOR) {
            return StringUtils.ANSI_BOLD + location.getLocation().getFileName() +
                    ":" + location.getLocation().getStartLine() +
                    ":" + location.getLocation().getStartColumn() +
                    ":" + StringUtils.ANSI_RED + " error: " + StringUtils.ANSI_RESET;
        } else {
            return location.getLocation().getFileName() +
                    ":" + location.getLocation().getStartLine() +
                    ":" + location.getLocation().getStartColumn() +
                    ": error: ";
        }
    }

    /**
     * Get a string representing the source with the underlined token
     *
     * @param location The location to generate the position text for
     * @return The string representing the token
     */
    private static String sourceText(Locatable location) {
        // Get the positional information
        int startLine = location.getLocation().getStartLine();
        int endLine = location.getLocation().getEndLine();
        int startCol = location.getLocation().getStartColumn();
        int endCol = location.getLocation().getEndColumn();

        String[] lines = location.getLocation().getLines(
                startLine - 1,
                endLine
        );
        return StringUtils.underlineSource(
                lines,
                startLine,
                startCol,
                endLine,
                endCol,
                StringUtils.ANSI_RED
        );
    }

    /**
     * Wrap an error message with the full error text (base text + position text)
     *
     * @param errorMessage The error message
     * @param location The location where there was an exception
     * @return The full error text
     */
    private static String fullErrorText(String errorMessage, Locatable location) {
        return baseErrorText(location) +
                errorMessage + "\n" +
                sourceText(location) + "\n";
    }

}
