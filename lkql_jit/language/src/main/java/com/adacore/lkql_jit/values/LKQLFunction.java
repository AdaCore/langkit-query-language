//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.values.bases.BasicLKQLValue;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.ArrayList;

/** This class represents the function values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLFunction extends BasicLKQLValue {

    // ----- Attributes -----

    /** The root node representing the function body. */
    public final RootNode rootNode;

    /** The closure for the function execution. */
    public final Closure closure;

    /** Name of the function. */
    public final String name;

    /** The documentation of the function. */
    public final String documentation;

    /** Names of the function parameters. */
    public final String[] parameterNames;

    /**
     * Default values of the function parameters (if a function parameter doesn't have any, the
     * value is 'null').
     */
    protected final Node[] parameterDefaultValues;

    /** The node representing the body of the function. */
    public final Node body;

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
        final RootNode rootNode,
        final Closure closure,
        final String name,
        final String documentation,
        final String[] parameterNames,
        final Node[] parameterDefaultValues,
        final Node body
    ) {
        this.rootNode = rootNode;
        this.closure = closure;
        this.name = name;
        this.documentation = documentation;
        this.parameterNames = parameterNames;
        this.parameterDefaultValues = parameterDefaultValues;
        this.body = body;
    }

    // ----- Instance methods -----

    /** Shortcut function to get the function associated call target. */
    public CallTarget getCallTarget() {
        return this.rootNode.getCallTarget();
    }

    // ----- Value methods -----

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

    public String lkqlDocumentation() {
        return this.documentation;
    }

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

    public Node[] getParameterDefaultValues() {
        return parameterDefaultValues;
    }

    public boolean hasClosure() {
        return closure != Closure.EMPTY;
    }

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof LKQLFunction other)) return false;
        return ObjectUtils.equals(this.rootNode, other.rootNode);
    }

    @Override
    public int hashCode() {
        return ObjectUtils.hashCode(rootNode);
    }
}
