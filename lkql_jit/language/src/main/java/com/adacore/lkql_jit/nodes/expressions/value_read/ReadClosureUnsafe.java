//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a value reading in the closure when we don't know if the value is defined.
 *
 * @author Hugo GUERRIER
 */
public final class ReadClosureUnsafe extends BaseRead {

    // ----- Attributes -----

    /** Symbol to read in the closure. */
    private final String name;

    // ----- Constructors -----

    /**
     * Create a new closure read unsafe node.
     *
     * @param location The location of the node in the source.
     * @param slot The closure slot to read.
     * @param name The name of the closure symbol.
     */
    public ReadClosureUnsafe(final SourceSection location, final int slot, final String name) {
        super(location, slot);
        this.name = name;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        final Object res = FrameUtils.readClosure(frame, this.slot);
        if (res == null) {
            throw LKQLRuntimeException.unknownSymbol(name, this);
        } else {
            return res;
        }
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
