//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.nodes.declarations.selector.SelectorArm;
import com.adacore.lkql_jit.runtime.values.LKQLDepthValue;
import com.adacore.lkql_jit.runtime.values.LKQLRecValue;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This root node represents a selector execution on a given node in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorRootNode extends MemoizedRootNode<LKQLDepthValue, LKQLRecValue> {

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

        // Get the depthVal and set it into the frame
        LKQLDepthValue value = (LKQLDepthValue) frame.getArguments()[1];

        // Try memoization
        if (this.isMemoized) {
            if (this.isMemoized(value)) {
                return this.getMemoized(value);
            }
        }

        if (this.thisSlot > -1 && this.depthSlot > -1) {
            FrameUtils.writeLocal(frame, this.thisSlot, value.value);
            FrameUtils.writeLocal(frame, this.depthSlot, ((Integer) value.depth).longValue());
        }

        // Prepare the result
        LKQLRecValue res = null;

        // Try to match an arm, if there is none, set the result to unit
        for (SelectorArm arm : this.arms) {
            res = arm.executeArm(frame, value);
            if (res != null) break;
        }
        if (res == null) {
            res = new LKQLRecValue(new Object[0], new Object[0]);
        }

        // Do the memoization cache addition
        if (this.isMemoized) {
            this.putMemoized(value, res);
        }

        // Return the result
        return res;
    }
}
