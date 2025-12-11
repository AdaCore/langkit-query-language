//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.adacore.lkql_jit.nodes.root_nodes.QueryComprehensionRootNode;
import com.adacore.lkql_jit.nodes.utils.CreateClosureNode;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.lists.LKQLQueryComprehension;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

@NodeChild(value = "source", type = Expr.class)
public abstract class QueryComprehension extends Expr {

    // ----- Attributes -----

    private final QueryComprehensionRootNode rootNode;

    // ----- Children -----

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private CreateClosureNode createClosureNode;

    // -----  Constructors -----

    @SuppressWarnings("this-escape")
    protected QueryComprehension(
        final SourceSection location,
        final FrameDescriptor frameDescriptor,
        final ClosureDescriptor closureDescriptor,
        final Pattern pattern,
        final Expr guard,
        final Expr result
    ) {
        super(location);
        this.rootNode = new QueryComprehensionRootNode(
            LKQLLanguage.getLanguage(this),
            frameDescriptor,
            pattern,
            guard,
            result
        );
        this.createClosureNode = new CreateClosureNode(closureDescriptor);
    }

    // ----- Execution methods -----

    @Specialization
    protected LKQLQueryComprehension onIterable(VirtualFrame frame, Iterable source) {
        return new LKQLQueryComprehension(this.rootNode, createClosureNode.execute(frame), source);
    }

    @Fallback
    protected void fallback(VirtualFrame frame, Object notIterable) {
        throw LKQLRuntimeException.wrongType(
            LKQLTypesHelper.LKQL_ITERABLE,
            LKQLTypesHelper.fromJava(notIterable),
            this.getSource()
        );
    }

    // ----- Class methods -----

    abstract Expr getSource();

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
