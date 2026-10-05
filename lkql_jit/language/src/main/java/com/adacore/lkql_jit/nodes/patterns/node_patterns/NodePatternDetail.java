//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents the base node for all pattern details. */
public abstract class NodePatternDetail extends LKQLNode {

    // ----- Constructors -----

    /**
     * Create a new node pattern detail.
     *
     * @param location The node location in the source.
     */
    protected NodePatternDetail(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        throw LKQLEngineException.shouldNotReachHere();
    }

    /** Execute the pattern detail on an object and return whether it matches. */
    public abstract boolean executeDetail(VirtualFrame frame, Object node);
}
