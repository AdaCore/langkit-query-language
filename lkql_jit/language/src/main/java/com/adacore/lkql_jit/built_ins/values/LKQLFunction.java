/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins.values;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents the function values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLFunction implements TruffleObject, LKQLValue {

    // ----- Attributes -----

    /** The root node representing the function body. */
    private final FunctionRootNode rootNode;

    /** The closure for the function exercution. */
    private final Closure closure;

    /** The name of the function. */
    @CompilerDirectives.CompilationFinal private String name;

    /** The documentation of the function. */
    private final String documentation;

    /** Names of the function parameters. */
    private final String[] parameterNames;

    /**
     * Default values of the function parameters (if a function parameter doesn't have any, the
     * value is 'null').
     */
    private final Expr[] parameterDefaultValues;

    // ----- Constructors -----

    /**
     * Create a new function value.
     *
     * @param rootNode The function root node.
     * @param closure The closure of the function.
     * @param name The name of the function.
     * @param documentation The documentation of the function.
     * @param parameterNames The names of the parameters.
     * @param parameterDefaultValues The default values of the parameters.
     */
    public LKQLFunction(
            final FunctionRootNode rootNode,
            final Closure closure,
            final String name,
            final String documentation,
            final String[] parameterNames,
            final Expr[] parameterDefaultValues) {
        this.rootNode = rootNode;
        this.closure = closure;
        this.name = name;
        this.documentation = documentation;
        this.parameterNames = parameterNames;
        this.parameterDefaultValues = parameterDefaultValues;
    }

    // ----- Getters ------

    public FunctionRootNode getRootNode() {
        return rootNode;
    }

    public Closure getClosure() {
        return closure;
    }

    public String getName() {
        return name;
    }

    public String[] getParameterNames() {
        return parameterNames;
    }

    public Expr[] getParameterDefaultValues() {
        return parameterDefaultValues;
    }

    // ----- Setters -----

    public void setName(String name) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        this.name = name;
    }

    // ----- Instance methods -----

    /** Shortcut function to get the function associated call target. */
    public CallTarget getCallTarget() {
        return this.rootNode.getCallTarget();
    }

    /** Shortcut function to get the LKQL node representing the body of the function. */
    public Expr getBody() {
        return this.rootNode.getBody();
    }

    // ----- Value methods -----

    /** Tell the interop API that the value has an associated language. */
    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    /** Give the LKQL language class to the interop library. */
    @ExportMessage
    Class<? extends TruffleLanguage<?>> getLanguage() {
        return LKQLLanguage.class;
    }

    /** Exported message to compare two LKQL functions. */
    @ExportMessage
    static class IsIdenticalOrUndefined {
        /** Compare two LKQL functions. */
        @Specialization
        protected static TriState onFunction(final LKQLFunction left, final LKQLFunction right) {
            if (left.rootNode == right.rootNode) return TriState.TRUE;
            else return TriState.FALSE;
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
                @SuppressWarnings("unused") final LKQLFunction receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL function. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    static int identityHashCode(final LKQLFunction receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return "function<" + this.name + ">";
    }

    /** Tell the interop library that the value is executable. */
    @ExportMessage
    boolean isExecutable() {
        return true;
    }

    /** Inner class for the function execution. */
    @ExportMessage
    static class Execute {
        /** Execute the function with the cached strategy. */
        @Specialization(guards = "callTarget == directCallNode.getCallTarget()")
        protected static Object doCached(
                final LKQLFunction function,
                final Object[] arguments,
                @Cached("function.getCallTarget()") CallTarget callTarget,
                @Cached("create(callTarget)") DirectCallNode directCallNode) {
            return directCallNode.call(arguments);
        }

        /** Execute the function with the uncached strategy. */
        @Specialization(replaces = "doCached")
        protected static Object doUncached(
                final LKQLFunction function,
                final Object[] arguments,
                @Cached() IndirectCallNode indirectCallNode) {
            return indirectCallNode.call(function.getCallTarget(), arguments);
        }
    }

    /** Tell the interop library that this value has an executable name. */
    @ExportMessage
    boolean hasExecutableName() {
        return true;
    }

    /** Return the function name to the interop library. */
    @ExportMessage
    Object getExecutableName() {
        return this.name;
    }

    // ----- LKQL values methods -----

    @Override
    public boolean internalEquals(LKQLValue o) {
        if (this == o) return true;
        if (!(o instanceof LKQLFunction other)) return false;
        return this.rootNode == other.rootNode;
    }

    @Override
    public String getDocumentation() {
        return this.documentation;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "function<" + this.name + ">";
    }
}
