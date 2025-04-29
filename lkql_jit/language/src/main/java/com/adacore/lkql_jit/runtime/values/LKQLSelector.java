//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.runtime.values.lists.LKQLSelectorList;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents the selector values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLSelector extends BasicLKQLValue {

    // ----- Attributes -----

    /** The root node containing the selector semantics. */
    private final SelectorRootNode rootNode;

    /** Closure for the selector execution. */
    private final Closure closure;

    /** The name of the selector. */
    private final String name;

    /** The documentation of the selector. */
    private final String documentation;

    // ----- Constructors -----

    /**
     * Create a new selector value.
     *
     * @param rootNode The root node of the selector.
     * @param closure The closure of the selector.
     * @param name The name of the selector.
     * @param documentation The documentation of the selector.
     */
    public LKQLSelector(
        final SelectorRootNode rootNode,
        final Closure closure,
        final String name,
        final String documentation
    ) {
        this.rootNode = rootNode;
        this.closure = closure;
        this.name = name;
        this.documentation = documentation;
    }

    // ----- Instance functions -----

    /** Execute the selector value. */
    public LKQLSelectorList getList(Object value) {
        return this.getList(value, -1, -1, -1);
    }

    /**
     * Execute the selector value on an ada node with additional arguments.
     *
     * @param maxDepth The maximum depth of the selector list.
     * @param minDepth The minimal depth of the selector list.
     * @param depth The precise depth to get.
     */
    public LKQLSelectorList getList(Object value, int maxDepth, int minDepth, int depth) {
        return new LKQLSelectorList(this.rootNode, this.closure, value, maxDepth, minDepth, depth);
    }

    // ----- Value methods -----

    /** Exported message to compare two LKQL selectors. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {

        /** Compare two LKQL selectors. */
        @Specialization
        protected static TriState onSelector(final LKQLSelector left, final LKQLSelector right) {
            return TriState.valueOf(left.rootNode == right.rootNode);
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
            @SuppressWarnings("unused") final LKQLSelector receiver,
            @SuppressWarnings("unused") final Object other
        ) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL selector. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public static int identityHashCode(final LKQLSelector receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @Override
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return "selector<" + this.name + ">";
    }

    /** Tell the interop library that the value is not executable. */
    @ExportMessage
    public boolean isExecutable() {
        // TODO (issue #143): Make the LKQLSelector executable as LKQLFunctions
        return false;
    }

    /** Placeholder function for the Truffle DSL. */
    @ExportMessage
    public Object execute(@SuppressWarnings("unused") Object[] arguments)
        throws UnsupportedTypeException, ArityException, UnsupportedMessageException {
        // TODO (issue #143): implement this method to execute the selector as a simple function
        // returning a selector list
        return null;
    }

    // ----- LKQL value methods -----

    @Override
    public String lkqlDocumentation() {
        return this.documentation;
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public String lkqlProfile() {
        return this.name + "()";
    }
}
