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

package com.adacore.lkql_jit.runtime.values;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLSelectorList;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.CompilerDirectives;

/**
 * This class represents the selector values in the LQKL language.
 *
 * @author Hugo GUERRIER
 */
public class SelectorValue implements LKQLValue {

    // ----- Attributes -----

    /** The root node of the selector. */
    private final SelectorRootNode rootNode;

    /** The closure of the selector. */
    private final Closure closure;

    /** The name of the selector. */
    private final String name;

    /** The documentation of the selector value. */
    private final String documentation;

    // ----- Constructors -----

    /**
     * Create a new selector value.
     *
     * @param selectorRootNode The root node of the selector.
     * @param closure The closure of the selector.
     * @param name The name of the selector.
     * @param documentation The documentation of the selector.
     */
    @CompilerDirectives.TruffleBoundary
    public SelectorValue(
            SelectorRootNode selectorRootNode, Closure closure, String name, String documentation) {
        this.rootNode = selectorRootNode;
        this.closure = closure;
        this.name = name;
        this.documentation = documentation;
    }

    // ----- Instance methods -----

    /**
     * Execute the selector value on an ada node.
     *
     * @param node The node to execute the selector on.
     * @return The selector list value.
     */
    public LKQLSelectorList execute(Libadalang.AdaNode node) {
        return this.execute(node, -1, -1, -1);
    }

    /**
     * Execute the selector value on an ada node with additional arguments.
     *
     * @param node The node to execute the selector on.
     * @param maxDepth The maximum depth of the selector list.
     * @param minDepth The minimal depth of the selector list.
     * @param depth The precise depth to get.
     * @return The selector list value.
     */
    public LKQLSelectorList execute(
            Libadalang.AdaNode node, int maxDepth, int minDepth, int depth) {
        return new LKQLSelectorList(this.rootNode, this.closure, node, maxDepth, minDepth, depth);
    }

    // ----- Value methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#getDocumentation()
     */
    @Override
    public String getDocumentation() {
        return this.documentation;
    }

    /**
     * @see
     *     com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    public boolean internalEquals(LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof SelectorValue other)) return false;
        return this.name.equals(other.name);
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return "selector<" + this.name + ">";
    }
}
