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
import com.adacore.lkql_jit.built_ins.values.bases.BasicLKQLValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;

/**
 * This class represents an ada node with the depth information. TODO : This value will change when
 * all Libadalang objects will be wrapped in interop LKQL values (#154).
 */
@ExportLibrary(InteropLibrary.class)
public final class LKQLDepthNode extends BasicLKQLValue {

    // ----- Attributes -----

    /** The depth of the node. */
    private final int depth;

    /** The decorated node. */
    private final Libadalang.AdaNode node;

    // ----- Constructors -----

    /** Create a new depth node. */
    public LKQLDepthNode(int depth, Libadalang.AdaNode node) {
        this.depth = depth;
        this.node = node;
    }

    // ----- Getters -----

    public int getDepth() {
        return depth;
    }

    public Libadalang.AdaNode getNode() {
        return node;
    }

    // ----- Value methods -----

    /** Exported message to compare two LKQL depth nodes. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {
        /** Compare two LKQL depth nodes. */
        @Specialization
        protected static TriState onNode(final LKQLDepthNode left, final LKQLDepthNode right) {
            return TriState.valueOf(left.node.equals(right.node));
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
                @SuppressWarnings("unused") final LKQLDepthNode receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL depth node. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public static int identityHashCode(final LKQLDepthNode receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @ExportMessage
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return this.node.toString();
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.node.toString();
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public int hashCode() {
        return this.node.hashCode();
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof LKQLDepthNode other)) return false;
        return this.node.equals(other.node);
    }
}
