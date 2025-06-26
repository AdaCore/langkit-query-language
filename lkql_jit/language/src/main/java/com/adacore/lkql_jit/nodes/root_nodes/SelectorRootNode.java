//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLRecValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This root node represents a selector execution on a given node in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorRootNode extends MemoizedRootNode<Object, LKQLRecValue> {

    // ----- Attributes -----

    /** Whether the selector is memoized. */
    @CompilerDirectives.CompilationFinal
    private final boolean isMemoized;

    private final String name;

    // ----- Children -----

    /** The body of the selector */
    @Child
    private Expr body;

    // ----- Constructors -----

    /**
     * Create a new selector root node.
     *
     * @param language The language instance to link the root node with.
     * @param frameDescriptor The frame descriptor for the root node.
     * @param isMemoized Whether the selector is memoized.
     */
    public SelectorRootNode(
        TruffleLanguage<?> language,
        FrameDescriptor frameDescriptor,
        boolean isMemoized,
        Expr body,
        String name
    ) {
        super(language, frameDescriptor);
        this.isMemoized = isMemoized;
        this.body = body;
        this.name = name;
    }

    // ----- Execution methods -----

    /**
     * Execute the selector on the given node, the first argument in the array. Return either : - A
     * Node if the result is only a node. - A Node[] if the result is an unpack of node. The return
     * value is wrapped in a selector call result record to have the mode information.
     */
    @Override
    public Object execute(VirtualFrame frame) {
        CompilerAsserts.compilationConstant(isMemoized);

        // Initialize the frame
        this.initFrame(frame);

        // Get the depthVal and set it into the frame
        Object value = frame.getArguments()[1];
        long depth = (long) frame.getArguments()[2];

        // Try memoization
        if (isMemoized && isMemoized(value)) {
            return this.getMemoized(value);
        }

        // Prepare the result
        LKQLRecValue res;

        var val = body.executeGeneric(frame);
        if (LKQLTypeSystemGen.isLKQLRecValue(val)) {
            res = LKQLTypeSystemGen.asLKQLRecValue(val);
            res.depth = (int) depth + 1;
        } else if (LKQLTypeSystemGen.isNullish(val)) {
            res = new LKQLRecValue(new Object[0], new Object[0]);
        } else {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_REC_VALUE,
                LKQLTypesHelper.fromJava(val),
                body
            );
        }

        // Do the memoization cache addition
        if (isMemoized) {
            putMemoized(value, res);
        }

        // Return the result
        return res;
    }

    @Override
    public String toString() {
        return (
            (this.body.getLocation().fileName() + ":" + this.body.getLocation().startLine()) +
            "::$" +
            this.name
        );
    }
}
