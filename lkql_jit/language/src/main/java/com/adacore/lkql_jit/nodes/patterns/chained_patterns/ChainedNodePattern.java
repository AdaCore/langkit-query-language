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

package com.adacore.lkql_jit.nodes.patterns.chained_patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.nodes.patterns.ValuePattern;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_functions.ListUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

import java.util.ArrayList;
import java.util.List;


/**
 * This node represents a chained node pattern in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ChainedNodePattern extends ValuePattern {

    // ----- Children -----

    /**
     * The node pattern start of the chain.
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern nodePattern;

    /**
     * The chain for the node pattern.
     */
    @Children
    private final ChainedPatternLink[] chain;

    // ----- Constructors -----

    /**
     * Create a new chained node pattern with parameters.
     *
     * @param location    The location of the node in the source.
     * @param nodePattern The node pattern.
     * @param chain       The chain for the node pattern.
     */
    public ChainedNodePattern(
        SourceLocation location,
        BasePattern nodePattern,
        ChainedPatternLink[] chain
    ) {
        super(location);
        this.nodePattern = nodePattern;
        this.chain = chain;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeNode(com.oracle.truffle.api.frame.VirtualFrame, com.adacore.libadalang.Libadalang.AdaNode)
     */
    @Override
    public boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node) {
        throw LKQLRuntimeException.wrongPatternType("ChainedNodePattern", this);
    }

    /**
     * Get the result node from the chained pattern.
     *
     * @param frame The frame to execute in.
     * @param node  The node to verify.
     * @return The result of the chained pattern.
     */
    public Libadalang.AdaNode[] executeChained(VirtualFrame frame, Libadalang.AdaNode node) {
        if (this.nodePattern.executeNode(frame, node)) {
            return this.executeLink(frame, 0, node);
        } else {
            return new Libadalang.AdaNode[0];
        }
    }

    // ----- Class methods -----

    /**
     * Execute the link of the chain at the given index with the given node.
     *
     * @param frame The frame to execute in.
     * @param index The index of the link to execute.
     * @param node  The node to execute the link on.
     * @return The array of the link execution result.
     */
    private Libadalang.AdaNode[] executeLink(VirtualFrame frame, int index, Libadalang.AdaNode node) {
        // Get the result of the link execution
        Libadalang.AdaNode[] linkRes = this.chain[index].executeLink(frame, node);

        // If the link is the last one return the result
        if (index >= this.chain.length - 1) {
            return linkRes;
        }

        // Else, execute the next link on the results of the current link
        else {
            // Prepare the result list
            List<Libadalang.AdaNode> resList = new ArrayList<>();

            // Add to the result with the already containing verification
            for (Libadalang.AdaNode linkNode : linkRes) {
                for (Libadalang.AdaNode toAdd : this.executeLink(frame, index + 1, linkNode)) {
                    if (!ListUtils.contains(resList, toAdd)) {
                        resList.add(toAdd);
                    }
                }
            }

            // Return the result
            return resList.toArray(new Libadalang.AdaNode[0]);
        }
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
