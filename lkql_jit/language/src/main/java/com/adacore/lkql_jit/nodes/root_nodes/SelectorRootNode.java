//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLDepthValue;
import com.adacore.lkql_jit.runtime.values.LKQLRecValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This root node represents a selector execution on a given node in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorRootNode extends MemoizedRootNode<LKQLDepthValue, LKQLRecValue> {

    // ----- Attributes -----

    /** The slot of the "this" variable. */
    @CompilerDirectives.CompilationFinal
    private final int thisSlot;

    /** The slot of the "depth" variable. */
    @CompilerDirectives.CompilationFinal
    private final int depthSlot;

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
     * @param thisSlot The slot to put the "this" variable.
     * @param depthSlot The slot to put the "depth" variable.
     */
    public SelectorRootNode(
        TruffleLanguage<?> language,
        FrameDescriptor frameDescriptor,
        boolean isMemoized,
        int thisSlot,
        int depthSlot,
        Expr body,
        String name
    ) {
        super(language, frameDescriptor);
        this.isMemoized = isMemoized;
        this.thisSlot = thisSlot;
        this.depthSlot = depthSlot;
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
        // Initialize the frame
        this.initFrame(frame);

        // Get the depthVal and set it into the frame
        LKQLDepthValue value = (LKQLDepthValue) frame.getArguments()[1];

        // Try memoization
        if (this.isMemoized && this.isMemoized(value)) {
            return this.getMemoized(value);
        }

        if (this.thisSlot > -1 && this.depthSlot > -1) {
            FrameUtils.writeLocal(frame, this.thisSlot, value.value);
            FrameUtils.writeLocal(frame, this.depthSlot, ((Integer) value.depth).longValue());
        }

        // Prepare the result
        LKQLRecValue res;

        var val = this.body.executeGeneric(frame);
        if (LKQLTypeSystemGen.isLKQLRecValue(val)) {
            res = LKQLTypeSystemGen.asLKQLRecValue(val);
            res.depth = value.depth + 1;
        } else if (LKQLTypeSystemGen.isNullish(val)) {
            res = new LKQLRecValue(new Object[0], new Object[0]);
        } else {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_REC_VALUE,
                LKQLTypesHelper.fromJava(val),
                this.body
            );
        }

        // Do the memoization cache addition
        if (this.isMemoized) {
            this.putMemoized(value, res);
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
