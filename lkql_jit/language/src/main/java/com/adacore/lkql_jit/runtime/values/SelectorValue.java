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
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorArm;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;


/**
 * This class represents the selector values in the LQKL language
 *
 * @author Hugo GUERRIER
 */
public class SelectorValue implements LKQLValue {

    // ----- Attributes -----

    /**
     * The name of the selector
     */
    private final String name;

    /**
     * The documentation of the selector value
     */
    private final String documentation;

    /**
     * The root node of the selector
     */
    private final SelectorRootNode rootNode;

    // ----- Constructors -----

    /**
     * Create a new selector value
     *
     * @param descriptor    The frame descriptor for the selector root node
     * @param closure       The closure
     * @param isMemoized    If the selector value is memoized
     * @param name          The name of the selector
     * @param documentation The documentation of the selector
     * @param thisSlot      The slot for the "this" variable
     * @param depthSlot     The slot for the "depth" variable
     * @param arms          The arms of the selector
     */
    @CompilerDirectives.TruffleBoundary
    public SelectorValue(
        FrameDescriptor descriptor,
        Closure closure,
        boolean isMemoized,
        String name,
        String documentation,
        int thisSlot,
        int depthSlot,
        SelectorArm[] arms
    ) {
        this.name = name;
        this.documentation = documentation;
        this.rootNode = new SelectorRootNode(
            LKQLLanguage.getLanguage(arms[0]),
            descriptor,
            closure,
            isMemoized,
            name,
            thisSlot,
            depthSlot,
            arms
        );
    }

    // ----- Class methods -----

    /**
     * Execute the selector value on an ada node
     *
     * @param node The node to execute the selector on
     * @return The selector list value
     */
    public SelectorListValue execute(Libadalang.AdaNode node) {
        return this.execute(node, -1, -1, -1);
    }

    /**
     * Execute the selector value on an ada node with additional arguments
     *
     * @param node     The node to execute the selector on
     * @param maxDepth The maximum depth of the selector list
     * @param minDepth The minimal depth of the selector list
     * @param depth    The precise depth to get
     * @return The selector list value
     */
    public SelectorListValue execute(Libadalang.AdaNode node, int maxDepth, int minDepth, int depth) {
        return new SelectorListValue(this.rootNode, node, maxDepth, minDepth, depth);
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
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
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