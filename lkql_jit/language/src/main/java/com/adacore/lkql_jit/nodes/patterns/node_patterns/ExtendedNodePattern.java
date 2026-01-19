//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a pattern that access fields or properties of nodes.
 *
 * @author Hugo GUERRIER
 */
public final class ExtendedNodePattern extends Pattern {

    // ----- Children -----

    /** The pattern to extend. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Pattern pattern;

    /** The details representing the extension. */
    @Children
    private final NodePatternDetail[] details;

    // ----- Constructors -----

    /**
     * Create a new extended node pattern node.
     *
     * @param location The location of the node in the source.
     * @param pattern The base pattern to extend.
     * @param details The extensions for the base pattern.
     */
    public ExtendedNodePattern(
        SourceSection location,
        Pattern pattern,
        NodePatternDetail[] details
    ) {
        super(location);
        this.pattern = pattern;
        this.details = details;
    }

    // ----- Execution methods -----

    /**
     * @see Pattern#executeValue(VirtualFrame, Object)
     */
    @Override
    @ExplodeLoop
    public boolean executeValue(VirtualFrame frame, Object value) {
        // Test the base pattern
        if (this.pattern.executeValue(frame, value)) {
            if (LKQLTypeSystemGen.isNodeInterface(value)) {
                var node = LKQLTypeSystemGen.asNodeInterface(value);

                // Verify all details
                for (int i = 0; i < details.length; i++) {
                    if (!details[i].executeDetail(frame, node)) return false;
                }

                // Return the success
                return true;
            } else if (LKQLTypeSystemGen.isDynamicAdaNode(value)) {
                var node = LKQLTypeSystemGen.asDynamicAdaNode(value);

                // Verify all details
                for (NodePatternDetail detail : this.details) {
                    if (!detail.executeDetail(frame, node)) return false;
                }

                // Return the success
                return true;
            } else {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.NODE_INTERFACE,
                    LKQLTypesHelper.fromJava(value),
                    this
                );
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
