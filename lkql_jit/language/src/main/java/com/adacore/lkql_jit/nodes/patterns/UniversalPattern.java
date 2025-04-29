//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the universal pattern in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class UniversalPattern extends ValuePattern {

    // ----- Constructors -----

    /**
     * Create a new universal pattern.
     *
     * @param location The location of the node in the source.
     */
    public UniversalPattern(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * @see BasePattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        return true;
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
