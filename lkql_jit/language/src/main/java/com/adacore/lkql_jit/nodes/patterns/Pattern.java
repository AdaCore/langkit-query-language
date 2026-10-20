//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents the base for all patterns in the LKQL language.  */
public abstract class Pattern extends LKQLNode {

    // ----- Constructors -----

    /** Create a new base pattern node. */
    protected Pattern(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        throw LKQLEngineException.shouldNotReachHere();
    }

    /** Execute the pattern and return whether the node fulfills it. */
    public abstract boolean executeValue(VirtualFrame frame, Object value);
}
