//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.LKQLContext;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a built-in reading. */
public final class ReadBuiltIn extends BaseRead {

    // ----- Constructors -----

    /**
     * Create a new built-in reading node.
     *
     * @param location The location of the node in the source.
     * @param slot The slot to read the built-in at.
     */
    public ReadBuiltIn(final SourceSection location, final int slot) {
        super(location, slot);
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return LKQLContext.get(this).getGlobal().getBuiltIn(this.slot);
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
