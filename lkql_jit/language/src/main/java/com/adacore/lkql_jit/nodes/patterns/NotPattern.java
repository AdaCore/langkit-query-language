//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents the negation of a given pattern in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NotPattern extends ValuePattern {

    // ----- Children -----

    /** The pattern to negate. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ValuePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new not pattern node.
     *
     * @param location The location of the node in the source.
     * @param pattern The pattern to negate.
     */
    public NotPattern(SourceLocation location, ValuePattern pattern) {
        super(location);
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * @see BasePattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        return !this.pattern.executeValue(frame, value);
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
