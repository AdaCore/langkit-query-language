//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.utils;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;

/** This node help for the conversion of a value to a rewriting node. */
public abstract class RewritingNodeConverter extends Node {

    /**
     * Convert the given value to a rewriting node, if this operation is possible. Else raise a
     * runtime error at the provided location.
     *
     * @param value The value to convert.
     * @param ensureUntied Whether to make sure the returned node is untied. If not, a clone is
     *     created.
     * @param usageLocation The location of the conversion in the LKQL sources.
     */
    public abstract Libadalang.RewritingNode execute(
        Object value,
        boolean ensureUntied,
        Node usageLocation
    );

    @Specialization
    protected Libadalang.RewritingNode onNull(
        @SuppressWarnings("unused") LKQLNull nullValue,
        @SuppressWarnings("unused") boolean ensureUntied,
        @SuppressWarnings("unused") Node usageLocation
    ) {
        return Libadalang.RewritingNode.NONE;
    }

    @Specialization(guards = "!node.isNone()")
    protected Libadalang.RewritingNode onNode(
        LangkitSupport.NodeInterface node,
        boolean ensureUntied,
        @SuppressWarnings("unused") Node usageLocation
    ) {
        final var res = node.getRewritingNode();
        return ensureUntied && res.isTied() ? res.clone() : res;
    }

    @Specialization
    protected Libadalang.RewritingNode onRewritingNode(
        Libadalang.RewritingNode node,
        boolean ensureUntied,
        @SuppressWarnings("unused") Node usageLocation
    ) {
        return ensureUntied && node.isTied() ? node.clone() : node;
    }

    @Fallback
    protected Libadalang.RewritingNode notNode(
        Object other,
        boolean ensureUntied,
        Node usageLocation
    ) {
        throw LKQLRuntimeException.wrongType(
            LKQLTypesHelper.typeUnion(
                LKQLTypesHelper.NODE_INTERFACE,
                LKQLTypesHelper.REWRITING_NODE
            ),
            LKQLTypesHelper.fromJava(other),
            usageLocation
        );
    }
}
