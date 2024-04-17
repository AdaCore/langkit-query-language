//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

public class SplatPattern extends ValuePattern {
    private final int slot;

    public SplatPattern(SourceLocation location) {
        super(location);
        this.slot = -1;
    }

    public SplatPattern(SourceLocation location, int slot) {
        super(location);
        this.slot = slot;
    }

    public boolean hasBinding() {
        return this.slot != -1;
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        if (hasBinding()) {
            // Do the node binding
            FrameUtils.writeLocal(frame, this.slot, value);
        }

        // Splat pattern always matches
        return true;
    }
}
