//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.LKQLContext;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a read in the prelude. */
public class ReadPrelude extends BaseRead {

    // ----- Constructors -----

    /** Create a new read prelude node. */
    public ReadPrelude(final SourceSection location, final int slot) {
        super(location, slot);
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return LKQLContext.get(this).getGlobal().preludeObjects[this.slot];
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
