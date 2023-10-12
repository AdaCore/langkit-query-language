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

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLSelectorList;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents the selector values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLSelector implements TruffleObject, LKQLValue {

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
            final String documentation) {
        this.rootNode = rootNode;
        this.closure = closure;
        this.name = name;
        this.documentation = documentation;
    }

    // ----- Instance functions -----

    /** Execute the selector value on an ada node. */
    public LKQLSelectorList getList(Libadalang.AdaNode node) {
        return this.getList(node, -1, -1, -1);
    }

    /**
     * Execute the selector value on an ada node with additional arguments.
     *
     * @param node The node to execute the selector on.
     * @param maxDepth The maximum depth of the selector list.
     * @param minDepth The minimal depth of the selector list.
     * @param depth The precise depth to get.
     */
    public LKQLSelectorList getList(
            Libadalang.AdaNode node, int maxDepth, int minDepth, int depth) {
        return new LKQLSelectorList(this.rootNode, this.closure, node, maxDepth, minDepth, depth);
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

    /** Exported message to compare two LKQL selectors. */
    @ExportMessage
    static class IsIdenticalOrUndefined {
        /** Compare two LKQL selectors. */
        @Specialization
        protected static TriState onSelector(final LKQLSelector left, final LKQLSelector right) {
            if (left.rootNode == right.rootNode) return TriState.TRUE;
            else return TriState.FALSE;
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
                @SuppressWarnings("unused") final LKQLSelector receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL selector. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    static int identityHashCode(final LKQLSelector receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return "selector<" + this.name + ">";
    }

    /** Tell the interop library that the value is not executable. */
    @ExportMessage
    boolean isExecutable() {
        // TODO (issue #143): Make the LKQLSelector executable as LKQLFunctions
        return false;
    }

    /** Placeholder function for the Truffle DSL. */
    @ExportMessage
    Object execute(Object[] arguments)
            throws UnsupportedTypeException, ArityException, UnsupportedMessageException {
        // TODO (issue #143): implement this method to execute the selector as a simple function
        // returning a selector list
        return null;
    }

    // ----- LKQL value methods -----

    @Override
    public boolean internalEquals(LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof LKQLSelector other)) return false;
        return this.rootNode == other.rootNode;
    }

    @Override
    public String getDocumentation() {
        return this.documentation;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "selector<" + this.name + ">";
    }
}
