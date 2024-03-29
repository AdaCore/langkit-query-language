//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.ValuePattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a pattern that access fields or properties of nodes.
 *
 * @author Hugo GUERRIER
 */
public final class ExtendedNodePattern extends NodePattern {

    // ----- Children -----

    /** The pattern to extend. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ValuePattern basePattern;

    /** The details representing the extension. */
    @Children private final NodePatternDetail[] details;

    // ----- Constructors -----

    /**
     * Create a new extended node pattern node.
     *
     * @param location The location of the node in the source.
     * @param basePattern The base pattern to extend.
     * @param details The extensions for the base pattern.
     */
    public ExtendedNodePattern(
            SourceLocation location, ValuePattern basePattern, NodePatternDetail[] details) {
        super(location);
        this.basePattern = basePattern;
        this.details = details;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeValue(VirtualFrame, Object)
     */
    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        // Test the base pattern
        if (this.basePattern.executeValue(frame, value)) {
            if (LKQLTypeSystemGen.isAdaNode(value)) {
                var node = LKQLTypeSystemGen.asAdaNode(value);

                // Verify all details
                for (NodePatternDetail detail : this.details) {
                    if (!detail.executeDetail(frame, node)) return false;
                }

                // Return the success
                return true;
            } else {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ADA_NODE, LKQLTypesHelper.fromJava(value), this);
            }
        }

        // Return the failure
        return false;
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
