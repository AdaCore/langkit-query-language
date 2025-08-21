//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.lkql_jit.built_ins.BuiltInPropertyValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.Constants;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node is used to wrap all dot access nodes and perform eventual post access processing. For
 * example, when accessing a built-in attribute, the value returned by the dot-access node is a
 * method reference which should be implicitly executed by this wrapper to stick to the LKQL
 * semantics.
 */
@NodeChild(value = "dotAccess", type = BaseDotAccess.class)
public abstract class DotAccessWrapper extends Expr {

    // ----- Constructors -----

    public DotAccessWrapper(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * When the child dot-access is returning an attribute, execute this attribute implicitly and
     * return the result of this execution.
     */
    @Specialization(
        limit = Constants.SPECIALIZED_LIB_LIMIT,
        guards = "attribute.getCallTarget() == directCallNode.getCallTarget()"
    )
    protected Object doCached(
        BuiltInPropertyValue attribute,
        @Cached("create(attribute.getCallTarget())") DirectCallNode directCallNode
    ) {
        return directCallNode.call(attribute.thisValue);
    }

    @Specialization(replaces = "doCached")
    protected Object doUncached(
        BuiltInPropertyValue attribute,
        @Cached IndirectCallNode indirectCallNode
    ) {
        return indirectCallNode.call(attribute.getCallTarget(), attribute.thisValue);
    }

    /** For other cases, just return the dot access result value. */
    @Fallback
    protected Object onOther(Object other) {
        return other;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
