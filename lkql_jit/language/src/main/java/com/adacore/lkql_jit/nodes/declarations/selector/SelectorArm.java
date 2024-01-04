//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations.selector;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.runtime.values.LKQLDepthValue;
import com.adacore.lkql_jit.runtime.values.LKQLRecValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents an arm for a selector declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorArm extends LKQLNode {

    // ----- Children -----

    /** The pattern to match. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    /** The expression to return if the arm is executed. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new selector arm.
     *
     * @param location The token location in the source.
     * @param pattern The pattern for the arm.
     * @param expr The expression to return.
     */
    public SelectorArm(SourceSection location, BasePattern pattern, Expr expr) {
        super(location);
        this.pattern = pattern;
        this.expr = expr;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the selector arm and return if the node match.
     *
     * @param frame The frame to execute the arm in.
     * @param node The node to match.
     * @return The result of the arm execution or null if the arm doesn't match.
     */
    public LKQLRecValue executeArm(VirtualFrame frame, LKQLDepthValue node) {
        if (this.pattern.executeValue(frame, node.value)) {
            final var expr = this.expr.executeGeneric(frame);

            if (LKQLTypeSystemGen.isLKQLRecValue(expr)) {
                LKQLRecValue val = LKQLTypeSystemGen.asLKQLRecValue(expr);
                val.depth = node.depth + 1;
                return val;
            } else if (LKQLTypeSystemGen.isNullish(expr)) {
                return new LKQLRecValue(new Object[0], new Object[0]);
            } else {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_REC_VALUE, LKQLTypesHelper.fromJava(expr), this.expr);
            }
        }
        // Return null if the arm hasn't been executed
        return null;
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
