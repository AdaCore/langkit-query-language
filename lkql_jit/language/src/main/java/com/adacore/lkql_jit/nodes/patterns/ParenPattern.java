//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a parented pattern in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ParenPattern extends ValuePattern {

    // ----- Children -----

    /** The pattern inside the parenthesis. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new parented pattern node.
     *
     * @param location The location of the node in the source.
     * @param pattern The pattern inside the parenthesis.
     */
    public ParenPattern(SourceSection location, BasePattern pattern) {
        super(location);
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * @see BasePattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        return this.pattern.executeValue(frame, value);
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
