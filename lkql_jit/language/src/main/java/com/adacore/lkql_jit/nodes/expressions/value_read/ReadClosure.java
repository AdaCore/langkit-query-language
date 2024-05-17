//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This class represents a closure value reading in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ReadClosure extends BaseRead {

    // ----- Constructors -----

    /**
     * Create a new closure reading node.
     *
     * @param location The location of the node in the source.
     * @param slot The slot index to read in the closure.
     */
    public ReadClosure(final SourceSection location, final int slot) {
        super(location, slot);
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return FrameUtils.readClosure(frame, this.slot);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"slot"}, new Object[] {this.slot});
    }
}
