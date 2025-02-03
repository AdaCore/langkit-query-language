//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public abstract class IntegerPattern extends ValuePattern {

    private final long toMatch;

    public IntegerPattern(SourceSection loc, long toMatch) {
        super(loc);
        this.toMatch = toMatch;
    }

    @Specialization
    public boolean onInt(@SuppressWarnings("unused") VirtualFrame frame, long val) {
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
