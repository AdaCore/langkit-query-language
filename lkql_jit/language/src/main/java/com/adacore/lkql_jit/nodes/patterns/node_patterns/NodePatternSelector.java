//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.adacore.lkql_jit.nodes.patterns.SelectorCall;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a pattern detail on a selector. */
public final class NodePatternSelector extends NodePatternDetail {

    // ----- Children -----

    /** The selector call for the detail. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private SelectorCall call;

    /** The pattern to check from the selector. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Pattern pattern;

    // ----- Constructors -----

    /**
     * Create a new node pattern selector detail node.
     *
     * @param location The location of the node in the source.
     * @param call The selector to use for iteration.
     * @param pattern The pattern to match values against.
     */
    public NodePatternSelector(SourceSection location, SelectorCall call, Pattern pattern) {
        super(location);
        this.call = call;
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    @Override
    public boolean executeDetail(VirtualFrame frame, Object node) {
        return this.call.executeVerification(frame, node, this.pattern);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
