//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.utilities.TriState;
import java.util.ArrayList;

/** This class represents the function values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLFunction extends BasicLKQLValue {

    // ----- Attributes -----

    /** The root node representing the function body. */
    public final FunctionRootNode rootNode;

    /** The closure for the function execution. */
    public final Closure closure;

    /** The documentation of the function. */
    public final String documentation;

    /** Names of the function parameters. */
    public final String[] parameterNames;

    /**
     * Default values of the function parameters (if a function parameter doesn't have any, the
     * value is 'null').
     */
    protected final Expr[] parameterDefaultValues;

    // ----- Constructors -----

    /**
     * Create a new function value.
     *
     * @param rootNode The function root node.
     * @param closure The closure of the function.
     * @param name The name of the function.
     * @param documentation The documentation of the function.
     * @param parameterNames The names of the parameters.
     */
    public LKQLFunction(
        final FunctionRootNode rootNode,
        final Closure closure,
        final String name,
        final String documentation,
        final String[] parameterNames,
        final Expr[] parameterDefaultValues
    ) {
        this.rootNode = rootNode;
        this.closure = closure;
        this.documentation = documentation;
        this.parameterNames = parameterNames;
        this.parameterDefaultValues = parameterDefaultValues;
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

    /**
     * Compute the final args to call this function, prepending the potential
     * closure argument if necessary.
     */
    public Object[] computeArgs(Object[] args) {
        if (closure != Closure.EMPTY) {
            return ArrayUtils.concat(new Object[] { closure.getContent() }, args);
        }
        return args;
    }

    // ----- Value methods -----

    /** Exported message to compare two LKQL functions. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {

        /** Compare two LKQL functions. */
        @Specialization
        protected static TriState onFunction(final LKQLFunction left, final LKQLFunction right) {
            return TriState.valueOf(left.rootNode == right.rootNode);
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
            @SuppressWarnings("unused") final LKQLFunction receiver,
            @SuppressWarnings("unused") final Object other
        ) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL function. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public static int identityHashCode(final LKQLFunction receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @Override
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return "function<" + this.rootNode.getName() + ">";
    }

    /** Tell the interop library that the value is executable. */
    @ExportMessage
    public boolean isExecutable() {
        return true;
    }

    /** Inner class for the function execution. */
    @ExportMessage
    public static class Execute {

        /** Execute the function with the cached strategy. */
        @Specialization(
            guards = "function.getCallTarget() == directCallNode.getCallTarget()",
            limit = Constants.SPECIALIZED_LIB_LIMIT
        )
        protected static Object doCached(
            @SuppressWarnings("unused") final LKQLFunction function,
            final Object[] arguments,
            @Cached("create(function.getCallTarget())") DirectCallNode directCallNode
        ) {
            return directCallNode.call(arguments);
        }

        /** Execute the function with the uncached strategy. */
        @Specialization(replaces = "doCached")
        protected static Object doUncached(
            final LKQLFunction function,
            final Object[] arguments,
            @Cached IndirectCallNode indirectCallNode
        ) {
            return indirectCallNode.call(function.getCallTarget(), arguments);
        }
    }

    /** Tell the interop library that this value has an executable name. */
    @ExportMessage
    public boolean hasExecutableName() {
        return true;
    }

    /** Return the function name to the interop library. */
    @ExportMessage
    public String getExecutableName() {
        return this.rootNode.getName();
    }

    // ----- LKQL values methods -----

    @Override
    public String lkqlDocumentation() {
        return this.documentation;
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public String lkqlProfile() {
        var expandedParams = new ArrayList<String>();
        for (int i = 0; i < parameterNames.length; i++) {
            var defVal = parameterDefaultValues[i];
            if (defVal != null) {
                expandedParams.add(
                    parameterNames[i] + "=" + defVal.getSourceSection().getCharacters().toString()
                );
            } else {
                expandedParams.add(parameterNames[i]);
            }
        }
        return (
            rootNode.getName() +
            "(" +
            String.join(", ", expandedParams.toArray(new String[0])) +
            ")"
        );
    }

    public Expr[] getParameterDefaultValues() {
        return parameterDefaultValues;
    }

    public boolean hasClosure() {
        return closure != Closure.EMPTY;
    }
}
