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
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;

/** This node help for the conversion of a value to a rewriting node. */
@GenerateInline(false)
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
    public abstract LangkitSupport.RewritingNodeInterface execute(
        Object value,
        boolean ensureUntied,
        Node usageLocation
    );

    @Specialization
    protected LangkitSupport.RewritingNodeInterface onNull(
        @SuppressWarnings("unused") LKQLNull nullValue,
        @SuppressWarnings("unused") boolean ensureUntied,
        @SuppressWarnings("unused") Node usageLocation
    ) {
        /*
         * TODO: Genericize LKQL issue or Java issue #502. Returning a "null" rewriting node requires to
         * have either access to the None object or to a static method getNONE(), which is not
         * feasible through Java interfaces nor abstract classes (I think). We could also simply
         * return "new LangkitSupport.RewritingNode()" but here again interfaces do not accept
         * constructors and I'm sure whether this is possible using abstract classes.
         * So we just create the object directly in the interface but I would prefer to call a constructor.
         */
        return Libadalang.RewritingNode.NONE;
    }

    @Specialization(guards = "!node.isNone()")
    protected LangkitSupport.RewritingNodeInterface onNode(
        LangkitSupport.NodeInterface node,
        boolean ensureUntied,
        @SuppressWarnings("unused") Node usageLocation
    ) {
        final var res = node.getRewritingNode();
        return ensureUntied && res.isTied() ? res.clone() : res;
    }

    @Specialization
    protected LangkitSupport.RewritingNodeInterface onRewritingNode(
        LangkitSupport.RewritingNodeInterface node,
        boolean ensureUntied,
        @SuppressWarnings("unused") Node usageLocation
    ) {
        return ensureUntied && node.isTied() ? node.clone() : node;
    }

    @Fallback
    protected LangkitSupport.RewritingNodeInterface notNode(
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
