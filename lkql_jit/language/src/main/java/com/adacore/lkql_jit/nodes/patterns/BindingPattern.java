//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a binding pattern in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class BindingPattern extends UnfilteredPattern {

    // ----- Attributes -----

    /** Frame slot to put the node in. */
    private final int slot;

    // ----- Children -----

    /** Pattern to execute once the binding done. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    public ValuePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new binding pattern node.
     *
     * @param location The location of the node in the source.
     * @param slot The frame slot to put the node in.
     * @param pattern The pattern to bind in.
     */
    public BindingPattern(SourceSection location, int slot, ValuePattern pattern) {
        super(location);
        this.slot = slot;
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * @see BasePattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        // Do the node binding
        FrameUtils.writeLocal(frame, this.slot, value);

        // Execute the pattern with the binding done
        return this.pattern == null || this.pattern.executeValue(frame, value);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "slot" },
                new Object[] { this.slot }
            );
    }
}
