//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.runtime.Cell;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node is the base of all LKQL root nodes.
 *
 * @author Hugo GUERRIER
 */
public abstract class BaseRootNode extends RootNode {

    /** Source location where this root node has been defined. */
    public final SourceSection location;

    /** Number of slot of the frame that this node is going to be called with. */
    @CompilerDirectives.CompilationFinal
    public final int frameSize;

    // ----- Constructors -----

    /**
     * Create a new base root node.
     *
     * @param language The language instance to link the root node with.
     * @param frameDescriptor The frame descriptor for the root node.
     */
    protected BaseRootNode(
        SourceSection location,
        final TruffleLanguage<?> language,
        final FrameDescriptor frameDescriptor
    ) {
        super(language, frameDescriptor);
        this.location = location;
        this.frameSize = frameDescriptor == null ? 0 : frameDescriptor.getNumberOfSlots();
    }

    // ----- Instance methods -----

    /**
     * Initialize the frame slots at empty cells.
     *
     * @param frame The frame to initialize.
     */
    @ExplodeLoop
    protected void initFrame(VirtualFrame frame) {
        for (int i = 0; i < frameSize; i++) {
            frame.setObject(i, new Cell());
        }
    }

    @Override
    public SourceSection getSourceSection() {
        return location;
    }
}
