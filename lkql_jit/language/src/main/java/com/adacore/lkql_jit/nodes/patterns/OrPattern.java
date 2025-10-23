//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the "or" pattern that combines two other patterns in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class OrPattern extends Pattern {

    // ----- Children -----

    /** The left part of the "or" pattern. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Pattern left;

    /** The right part of the "or" pattern. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Pattern right;

    // ----- Constructors -----

    /**
     * Create a new "or" pattern node.
     *
     * @param location The location of the node in the source.
     * @param left The left part of the "or".
     * @param right The right part of the "or".
     */
    public OrPattern(SourceSection location, Pattern left, Pattern right) {
        super(location);
        this.left = left;
        this.right = right;
    }

    // ----- Execution methods -----

    /**
     * @see Pattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        // Do the short circuit
        if (this.left.executeValue(frame, value)) {
            return true;
        } else {
            return this.right.executeValue(frame, value);
        }
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
