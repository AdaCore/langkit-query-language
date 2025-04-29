//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.runtime.Cell;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * This node is the base of all LKQL root nodes.
 *
 * @author Hugo GUERRIER
 */
public abstract class BaseRootNode extends RootNode {

    // ----- Constructors -----

    /**
     * Create a new base root node.
     *
     * @param language The language instance to link the root node with.
     * @param frameDescriptor The frame descriptor for the root node.
     */
    protected BaseRootNode(
        final TruffleLanguage<?> language,
        final FrameDescriptor frameDescriptor
    ) {
        super(language, frameDescriptor);
    }

    // ----- Instance methods -----

    /**
     * Get the call target as a generic Truffle calling interface.
     *
     * @return The call target.
     */
    public CallTarget getRealCallTarget() {
        return this.getCallTarget();
    }

    /**
     * Initialize the frame slots at empty cells.
     *
     * @param frame The frame to initialize.
     */
    protected void initFrame(VirtualFrame frame) {
        final int slotNumber = frame.getFrameDescriptor().getNumberOfSlots();
        for (int i = 0; i < slotNumber; i++) {
            frame.setObject(i, new Cell());
        }
    }
}
