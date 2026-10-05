//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a local variable reading. */
public final class ReadLocal extends BaseRead {

    // ----- Constructors -----

    /**
     * Create a new local value reading node.
     *
     * @param location The location of the node in the source.
     * @param slot The slot index to read.
     */
    public ReadLocal(final SourceSection location, final int slot) {
        super(location, slot);
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return FrameUtils.readLocal(frame, this.slot);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[] { "slot" },
            new Object[] { this.slot }
        );
    }
}
