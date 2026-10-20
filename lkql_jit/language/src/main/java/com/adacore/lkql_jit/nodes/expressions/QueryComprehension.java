//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ClosureDescriptor;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.adacore.lkql_jit.nodes.root_nodes.QueryComprehensionRootNode;
import com.adacore.lkql_jit.nodes.utils.CreateClosureNode;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.streams.LKQLQueryComprehension;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a lazy query comprehension. */
@NodeChild(value = "source", type = Expr.class)
public abstract class QueryComprehension extends Expr {

    // ----- Attributes -----

    private final QueryComprehensionRootNode rootNode;

    // ----- Children -----

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private CreateClosureNode createClosureNode;

    // -----  Constructors -----

    /**
     * Create a new query comprehension node.
     *
     * @param location The location of the node in the source.
     * @param frameDescriptor Description of the frame to execute the processing in.
     * @param closureDescriptor Description of closed values read in this execution.
     * @param pattern The pattern to filter value in the source.
     * @param guard The guard to filter values.
     * @param result Mapping expression.
     */
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
            location,
            LKQLLanguage.get(this),
            frameDescriptor,
            pattern,
            guard,
            result
        );
        this.createClosureNode = new CreateClosureNode(closureDescriptor);
    }

    // ----- Getters -----

    abstract Expr getSource();

    // ----- Execution methods -----

    @Specialization
    protected LKQLQueryComprehension onIterable(VirtualFrame frame, Iterable source) {
        return new LKQLQueryComprehension(this.rootNode, createClosureNode.execute(frame), source);
    }

    @Fallback
    protected void fallback(VirtualFrame frame, Object notIterable) {
        throw LKQLRuntimeError.wrongType(
            LKQLTypesHelper.LKQL_ITERABLE,
            LKQLTypesHelper.fromJava(notIterable),
            this.getSource()
        );
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
