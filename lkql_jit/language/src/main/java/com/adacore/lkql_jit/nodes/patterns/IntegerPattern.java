//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a pattern to match integer values. */
public abstract class IntegerPattern extends Pattern {

    // ----- Attributes -----

    /** Value of the pattern. */
    private final long toMatch;

    // ----- Constructors -----

    /**
     * Create a new integer pattern node.
     *
     * @param location The location of the node in the source.
     */
    public IntegerPattern(SourceSection loc, long toMatch) {
        super(loc);
        this.toMatch = toMatch;
    }

    // ----- Execution methods -----

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

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[] { "toMatch" },
            new Object[] { this.toMatch }
        );
    }
}
