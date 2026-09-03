//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.values.LKQLNull;
import com.oracle.truffle.api.dsl.Executed;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class CastExpr extends Expr {

    // ----- Children -----

    /** The expression to cast. */
    @Child
    @Executed
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr expr;

    /** The node type it should be cast to. */
    protected final Class<? extends LangkitSupport.NodeInterface> nodeClass;

    /** Whether the execution should raise an exception if the cast fails, rather
     * than returning the null node. */
    protected final boolean strict;

    public CastExpr(
        SourceSection location,
        Expr expr,
        Class<? extends LangkitSupport.NodeInterface> nodeClass,
        boolean strict
    ) {
        super(location);
        this.expr = expr;
        this.nodeClass = nodeClass;
        this.strict = strict;
    }

    // ----- Execution methods -----

    public LangkitSupport.NodeInterface executeNode(VirtualFrame frame) {
        var result = this.expr.executeGeneric(frame);
        if (nodeClass.isAssignableFrom(result.getClass())) {
            return (LangkitSupport.NodeInterface) result;
        } else if (strict) {
            throw LKQLRuntimeError.castError(result.getClass(), nodeClass, this);
        } else {
            return LKQLNull.INSTANCE;
        }
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return executeNode(frame);
    }

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
