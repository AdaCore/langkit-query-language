//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.values.LKQLNull;
import com.adacore.lkql_jit.values.LKQLUnit;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a pattern that only matches on null values. */
public final class NullPattern extends Pattern {

    // ----- Constructors -----

    /**
     * Create a new null pattern node.
     *
     * @param location The location of the node in the source.
     */
    public NullPattern(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        return value == LKQLNull.INSTANCE || value == LKQLUnit.INSTANCE;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
