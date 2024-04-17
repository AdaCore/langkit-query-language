//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents the base node for all pattern detail in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class NodePatternDetail extends LKQLNode {

    /**
     * Create a new node pattern detail.
     *
     * @param location The token location in the source.
     */
    protected NodePatternDetail(SourceLocation location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the pattern detail on a node and return if the detail is valid.
     *
     * @param frame The frame to execute the detail in.
     * @param node The node to test in the detail.
     * @return True if the detail is valid, false else.
     */
    public abstract boolean executeDetail(VirtualFrame frame, Libadalang.AdaNode node);
}
