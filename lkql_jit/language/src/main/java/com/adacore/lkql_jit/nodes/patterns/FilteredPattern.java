//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBoolean;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBooleanNodeGen;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a pattern with a filter in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class FilteredPattern extends BasePattern {

    // ----- Children -----

    /** The pattern to filter. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private UnfilteredPattern pattern;

    /** The predicate to do the filtering. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr predicate;

    @Child
    private LKQLToBoolean toBoolean;

    // ----- Constructors -----

    /**
     * Create a new filtered pattern node.
     *
     * @param location The location of the node in the source.
     * @param pattern The pattern to filter.
     * @param predicate The predicate expression.
     */
    public FilteredPattern(SourceSection location, UnfilteredPattern pattern, Expr predicate) {
        super(location);
        this.pattern = pattern;
        this.predicate = predicate;
        this.toBoolean = LKQLToBooleanNodeGen.create();
    }

    // ----- Execution methods -----

    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        // If the pattern match, execute the predicate
        if (this.pattern.executeValue(frame, value)) {
            // Try to execute the predicate in a boolean
            return toBoolean.execute(predicate.executeGeneric(frame));
        }

        // Return the failure
        return false;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
