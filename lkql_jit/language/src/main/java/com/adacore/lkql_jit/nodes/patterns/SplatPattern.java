//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a pattern that splat remaining values in a collection. */
public class SplatPattern extends Pattern {

    // ----- Attributes -----

    /** The slot to place the constructed object in. */
    private final int slot;

    // ----- Constructors -----

    public SplatPattern(SourceSection location, int slot) {
        super(location);
        this.slot = slot;
    }

    // ----- Execution methods -----

    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        if (hasBinding()) {
            // Do the node binding
            FrameUtils.writeLocal(frame, this.slot, value);
        }

        // Splat pattern always matches
        return true;
    }

    // ----- Instance methods -----

    public boolean hasBinding() {
        return this.slot != -1;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
