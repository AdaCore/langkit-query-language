//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.lkql_jit.runtime.values.DynamicAdaNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the pattern that filters by node kind in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class DynamicNodeKindPattern extends NodePattern {

    // ----- Attributes -----

    /** The Java class of the node kind. */
    private final String kindName;

    // ----- Constructors -----

    /**
     * Create a new node kind pattern node.
     *
     * @param location The location of the node in the source.
     * @param kindName The node kind name.
     */
    public DynamicNodeKindPattern(SourceSection location, String kindName) {
        super(location);
        this.kindName = kindName;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        if (value instanceof DynamicAdaNode node) {
            return node.kind.equals(kindName);
        } else return false;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "kindName" },
                new Object[] { this.kindName }
            );
    }
}
