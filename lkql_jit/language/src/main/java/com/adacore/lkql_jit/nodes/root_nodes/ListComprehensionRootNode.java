//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This root node represents a list comprehension execution (expression and predicate) in the LKQL
 * language.
 *
 * @author Hugo GUERRIER
 */
public final class ListComprehensionRootNode extends BaseRootNode {

    /** The predicate of the list comprehension. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr predicate;

    /** The result expression of the list comprehension. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr result;

    // ----- Constructors -----

    /**
     * Create a new list comprehension root node.
     *
     * @param language The language instance to link the root node with.
     * @param frameDescriptor The frame descriptor for the root node.
     * @param predicate The predicate of the list comprehension.
     * @param result The result expression of the list comprehension.
     */
    public ListComprehensionRootNode(
        TruffleLanguage<?> language,
        FrameDescriptor frameDescriptor,
        Expr predicate,
        Expr result
    ) {
        super(language, frameDescriptor);
        this.predicate = predicate;
        this.result = result;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.root_nodes.BaseRootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object execute(VirtualFrame frame) {
        // Initialize the frame
        this.initFrame(frame);

        // Evaluate the predicate and return the result if true, else just return null
        try {
            if (this.predicate == null || this.predicate.executeTruthy(frame).isTruthy()) {
                return this.result.executeGeneric(frame);
            } else {
                return null;
            }
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_BOOLEAN,
                LKQLTypesHelper.fromJava(e.getResult()),
                this.predicate
            );
        }
    }
}
