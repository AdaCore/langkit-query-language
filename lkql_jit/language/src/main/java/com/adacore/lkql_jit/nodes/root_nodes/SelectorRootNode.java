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

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.built_ins.values.LKQLDepthValue;
import com.adacore.lkql_jit.built_ins.values.LKQLUnit;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorArm;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorExpr;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This root node represents a selector execution on a given node in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorRootNode
        extends MemoizedRootNode<LKQLDepthValue, SelectorRootNode.SelectorCallResult> {

    // ----- Attributes -----

    /** The slot of the "this" variable. */
    private final int thisSlot;

    /** The slot of the "depth" variable. */
    private final int depthSlot;

    /** Whether the selector is memoized. */
    private final boolean isMemoized;

    // ----- Children -----

    /** The selector arms. */
    @Children private final SelectorArm[] arms;

    // ----- Constructors -----

    /**
     * Create a new selector root node.
     *
     * @param language The language instance to link the root node with.
     * @param frameDescriptor The frame descriptor for the root node.
     * @param isMemoized Whether the selector is memoized.
     * @param thisSlot The slot to put the "this" variable.
     * @param depthSlot The slot to put the "depth" variable.
     * @param arms The arms of the selector.
     */
    public SelectorRootNode(
            TruffleLanguage<?> language,
            FrameDescriptor frameDescriptor,
            boolean isMemoized,
            int thisSlot,
            int depthSlot,
            SelectorArm[] arms) {
        super(language, frameDescriptor);
        this.isMemoized = isMemoized;
        this.thisSlot = thisSlot;
        this.depthSlot = depthSlot;
        this.arms = arms;
    }

    // ----- Execution methods -----

    /**
     * Execute the selector on the given node, the first argument in the array. Return either : - A
     * Node if the result is only a node. - A Node[] if the result is an unpack of node. The return
     * value is wrapped in a selector call result record to have the mode information.
     *
     * @see com.oracle.truffle.api.nodes.RootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object execute(VirtualFrame frame) {
        // Initialize the frame
        this.initFrame(frame);

        // Get the node and set it into the frame
        LKQLDepthValue value = (LKQLDepthValue) frame.getArguments()[1];

        // Try memoization
        if (this.isMemoized) {
            if (this.isMemoized(value)) return this.getMemoized(value);
        }

        if (this.thisSlot > -1 && this.depthSlot > -1) {
            FrameUtils.writeLocal(frame, this.thisSlot, value.value);
            FrameUtils.writeLocal(frame, this.depthSlot, ((Integer) value.depth).longValue());
        }

        // Prepare the result
        SelectorCallResult res = null;

        // Try to match an arm, if there is none, set the result to unit
        for (SelectorArm arm : this.arms) {
            res = arm.executeArm(frame, value);
            if (res != null) break;
        }
        if (res == null) {
            res = new SelectorCallResult(SelectorExpr.Mode.DEFAULT, LKQLUnit.INSTANCE);
        }

        // Do the memoization cache addition
        if (this.isMemoized) {
            this.putMemoized(value, res);
        }

        // Return the result
        return res;
    }

    // ----- Inner classes -----

    /**
     * This record represents a result of a call to the selector on a node.
     *
     * @param mode The mode of the result.
     * @param result The result value.
     */
    public record SelectorCallResult(SelectorExpr.Mode mode, Object result) {}
}
