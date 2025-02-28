//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** Pattern to match a boolean value */
public abstract class BoolPattern extends ValuePattern {

    private final boolean toMatch;

    protected BoolPattern(SourceSection location, boolean toMatch) {
        super(location);
        this.toMatch = toMatch;
    }

    @Specialization
    public boolean onBool(@SuppressWarnings("unused") VirtualFrame frame, boolean val) {
        return val == toMatch;
    }

    @Fallback
    public boolean onOther(
        @SuppressWarnings("unused") VirtualFrame frame,
        @SuppressWarnings("unused") Object other
    ) {
        return false;
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "toMatch" },
                new Object[] { this.toMatch }
            );
    }
}
