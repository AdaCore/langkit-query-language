//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
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
    }

    // ----- Execution methods -----

    @Override
    public boolean executeValue(VirtualFrame frame, Object value) {
        // If the pattern match, execute the predicate
        if (this.pattern.executeValue(frame, value)) {
            // Try to execute the predicate in a boolean
            try {
                return this.predicate.executeTruthy(frame).isTruthy();
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.predicate
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
