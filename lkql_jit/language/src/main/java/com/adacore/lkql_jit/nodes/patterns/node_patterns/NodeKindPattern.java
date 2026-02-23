//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.adacore.lkql_jit.values.LKQLNull;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the pattern that filters by node kind in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NodeKindPattern extends Pattern {

    // ----- Attributes -----

    /** The Java class of the node kind. */
    private final Class<? extends LangkitSupport.NodeInterface> nodeClazz;

    // ----- Constructors -----

    /**
     * Create a new node kind pattern node.
     *
     * @param location The location of the node in the source.
     * @param nodeClazz The node class to match.
     */
    public NodeKindPattern(
        SourceSection location,
        Class<? extends LangkitSupport.NodeInterface> nodeClazz
    ) {
        super(location);
        this.nodeClazz = nodeClazz;
    }

    // ----- Execution methods -----

    /**
     * @see Pattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        return this.nodeClazz.isInstance(value) && value != LKQLNull.INSTANCE;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[] { "nodeClazz" },
            new Object[] { this.nodeClazz.getSimpleName() }
        );
    }
}
