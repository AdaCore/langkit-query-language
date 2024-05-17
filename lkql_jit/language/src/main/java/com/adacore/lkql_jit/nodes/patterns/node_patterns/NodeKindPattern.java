//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.values.LKQLNull;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the pattern that filters by node kind in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NodeKindPattern extends NodePattern {

    // ----- Attributes -----

    /** The Java class of the node kind. */
    private final Class<? extends Libadalang.AdaNode> nodeClazz;

    // ----- Constructors -----

    /**
     * Create a new node kind pattern node.
     *
     * @param location The location of the node in the source.
     * @param kindName The node kind name.
     */
    public NodeKindPattern(SourceSection location, String kindName) {
        super(location);
        final var description = Libadalang.NODE_DESCRIPTION_MAP.get(kindName);
        if (description == null) {
            throw LKQLRuntimeException.invalidKindName(this);
        }
        this.nodeClazz = description.clazz;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeValue(VirtualFrame, Object)
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
                new String[] {"nodeClazz"},
                new Object[] {this.nodeClazz.getSimpleName()});
    }
}
