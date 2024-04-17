//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.nodes.patterns.SelectorCall;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a pattern detail on a selector in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NodePatternSelector extends NodePatternDetail {

    // ----- Children ------

    /** The selector call for the detail. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private SelectorCall call;

    /** The pattern to check from the selector. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new node pattern selector detail node.
     *
     * @param location The location of the node in the source.
     * @param call The selector call.
     * @param pattern The pattern to check the selector.
     */
    public NodePatternSelector(SourceLocation location, SelectorCall call, BasePattern pattern) {
        super(location);
        this.call = call;
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.patterns.node_patterns.NodePatternDetail#executeDetail(com.oracle.truffle.api.frame.VirtualFrame,
     *     com.adacore.libadalang.Libadalang.AdaNode)
     */
    @Override
    public boolean executeDetail(VirtualFrame frame, Libadalang.AdaNode node) {
        return this.call.executeVerification(frame, node, this.pattern);
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
