//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ClosureDescriptor;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.LazyCell;
import com.adacore.lkql_jit.nodes.utils.CreateClosureNode;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.source.SourceSection;

/** This class is a base class for stream operators, containing all common logic. */
@NodeChild(value = "head", type = Expr.class)
public abstract class BaseStreamOp extends Expr {

    // ----- Attributes -----

    /** Expression representing the tail. */
    private final Expr tail;

    /** Descriptor to create the tail frame. */
    private final FrameDescriptor tailFrameDescriptor;

    /** Root node to get the value of the tail. */
    protected final LazyCell tailLazyValue;

    // ----- Children -----

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected CreateClosureNode createTailClosure;

    // ----- Constructors -----

    @SuppressWarnings("this-escape")
    protected BaseStreamOp(
        SourceSection location,
        Expr tail,
        FrameDescriptor tailFrameDescriptor,
        ClosureDescriptor tailClosureDescriptor
    ) {
        super(location);
        this.tail = tail;
        this.tailFrameDescriptor = tailFrameDescriptor;
        this.tailLazyValue = new LazyCell(
            LKQLLanguage.getLanguage(this),
            this.tailFrameDescriptor,
            this.tail
        );
        this.createTailClosure = new CreateClosureNode(tailClosureDescriptor);
    }

    // ----- Getters -----

    public abstract Expr getHead();

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "head", "tail" },
                new Object[] { this.getHead(), this.tailLazyValue }
            );
    }
}
