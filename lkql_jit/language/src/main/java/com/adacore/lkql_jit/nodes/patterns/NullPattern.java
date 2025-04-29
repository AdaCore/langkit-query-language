//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the null pattern in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NullPattern extends ValuePattern {

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

    /**
     * @see BasePattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        return value == LKQLNull.INSTANCE || value == LKQLUnit.INSTANCE;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
