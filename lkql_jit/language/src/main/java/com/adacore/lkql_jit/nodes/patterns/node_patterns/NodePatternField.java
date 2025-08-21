//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.runtime.values.LKQLProperty;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a pattern detail on a field in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class NodePatternField extends NodePatternDetail {

    // ----- Attributes -----

    /** The name of the field to get. */
    protected final String fieldName;

    // ----- Children -----

    /** The expected value for the field. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected BasePattern expected;

    // ----- Constructors -----

    /**
     * Create a new node pattern field detail node.
     *
     * @param location The location of the node in the source.
     * @param fieldName The name of the field to get.
     * @param expected The expected value for the field.
     */
    protected NodePatternField(SourceSection location, String fieldName, BasePattern expected) {
        super(location);
        this.fieldName = fieldName;
        this.expected = expected;
    }

    // ----- Execution methods -----

    /**
     * Execute the field detail with the cached path.
     *
     * @param frame The frame to execute in.
     * @param node The node get the field from.
     * @param property The cached property reference.
     * @return True if the detail is valid, false else.
     */
    @Specialization(guards = { "node == property.node", "property.description != null" })
    protected boolean fieldCached(
        VirtualFrame frame,
        @SuppressWarnings("unused") LangkitSupport.NodeInterface node,
        @Cached("create(fieldName, node)") LKQLProperty property
    ) {
        // Get the value of the field
        Object value = property.executeAsField(this);

        // Verify if the detail value match
        return this.expected.executeValue(frame, value);
    }

    /**
     * Execute the field detail with the un-cached path.
     *
     * @param frame The frame to execute in.
     * @param node The node get the field from.
     * @return True if the detail is valid, false else.
     */
    @Specialization(replaces = "fieldCached")
    protected boolean fieldUncached(VirtualFrame frame, LangkitSupport.NodeInterface node) {
        // Get the field property reference
        LKQLProperty property = new LKQLProperty(this.fieldName, node);

        // Verify if the field method is null
        if (property.description == null) {
            throw LKQLRuntimeException.noSuchField(this);
        }

        // Execute the field detail
        return this.fieldCached(frame, node, property);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "fieldName" },
                new Object[] { this.fieldName }
            );
    }
}
