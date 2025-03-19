//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.runtime.Cell;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This class represents a closure value reading in the LKQL language.
 */
public abstract class ReadClosure extends BaseRead {

    /**
     * Whether the accessed entity is known to be package global or not. If it
     * is, we can optimize access to it, because package level entities cannot
     * change, so we can read them only once.
     */
    public final boolean isPackageGlobal;

    protected ReadClosure(final SourceSection location, final int slot, boolean isPackageGlobal) {
        super(location, slot);
        this.isPackageGlobal = isPackageGlobal;
    }

    /*
     * Optimization strategy:
     *
     * 1. If the entity is package global, read only once, and then always return this result
     * 2. Otherwise, we cache the closure object and optimize the read. We
     *    re-read the slot only if the closure object changes.
     */

    @Specialization(guards = "isPackageGlobal == true")
    public Object onGlobal(
        VirtualFrame frame,
        @Cached(value = "getValue(frame)", neverDefault = true) Object result
    ) {
        return result;
    }

    @Specialization(
        guards = "cachedClosure == getClosure(frame)",
        limit = Constants.SPECIALIZED_LIB_LIMIT
    )
    public Object onCachedClosure(
        VirtualFrame frame,
        @SuppressWarnings("unused") @Cached(
            value = "getClosure(frame)",
            dimensions = 1
        ) Cell[] cachedClosure,
        @Cached("getValue(frame)") Object result
    ) {
        return result;
    }

    @Specialization(replaces = "onCachedClosure")
    public Object OnUncachedClosure(VirtualFrame frame) {
        return onCachedClosure(frame, getClosure(frame), getValue(frame));
    }

    public Object getValue(VirtualFrame frame) {
        return FrameUtils.readClosure(frame, this.slot);
    }

    protected Cell[] getClosure(VirtualFrame frame) {
        return (Cell[]) frame.getArguments()[0];
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "slot" },
                new Object[] { this.slot }
            );
    }
}
