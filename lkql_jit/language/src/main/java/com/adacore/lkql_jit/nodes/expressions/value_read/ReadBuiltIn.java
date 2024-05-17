//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.LKQLLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a built-in reading in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
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

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return LKQLLanguage.getContext(this).getGlobal().getBuiltIn(this.slot);
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
