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

package com.adacore.lkql_jit.runtime.values;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.CompilerDirectives;


/**
 * This class represents an ada node with the depth information
 *
 * @author Hugo GUERRIER
 */
public final class DepthNode implements LKQLValue {

    // ----- Attributes -----

    /**
     * The depth of the node
     */
    private final int depth;

    /**
     * The decorated node
     */
    private final Libadalang.AdaNode node;

    // ----- Constructors -----

    /**
     * Create a new depth node
     *
     * @param depth The depth of the node
     * @param node  The node
     */
    public DepthNode(
        int depth,
        Libadalang.AdaNode node
    ) {
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

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    public boolean internalEquals(LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof DepthNode other)) return false;
        return other.node.equals(this.node);
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
        if (!(o instanceof DepthNode other)) return false;
        return this.node.equals(other.node);
    }

}
