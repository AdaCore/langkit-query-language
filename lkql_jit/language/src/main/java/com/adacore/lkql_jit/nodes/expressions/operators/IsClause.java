//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents the is clause in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "nodeExpr", type = Expr.class)
public abstract class IsClause extends Expr {

    // ----- Attributes -----

    /** The location of the node expression. */
    private final DummyLocation nodeLocation;

    // ----- Children -----

    /** The pattern node to evaluate the "is" clause. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new "is" clause with the parameters.
     *
     * @param location The token location in the source.
     * @param nodeLocation The location of the node expression node.
     * @param pattern The pattern to execute the is clause.
     */
    protected IsClause(SourceLocation location, DummyLocation nodeLocation, BasePattern pattern) {
        super(location);
        this.nodeLocation = nodeLocation;
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * Execute the is clause when the expression is a node.
     *
     * @param frame The frame to execute the pattern in.
     * @param node The node to verify.
     * @return The result of the pattern execution.
     */
    @Specialization
    protected boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node) {
        return this.pattern.executeValue(frame, node);
    }

    /**
     * Fallback method if the left operand is not a node.
     *
     * @param notNode The object that is not a node.
     */
    @Fallback
    protected void notNode(Object notNode) {
        throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.ADA_NODE, LKQLTypesHelper.fromJava(notNode), this.nodeLocation);
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
