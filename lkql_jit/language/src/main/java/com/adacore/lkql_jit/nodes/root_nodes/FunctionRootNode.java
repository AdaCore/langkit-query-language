//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.Arrays;

/**
 * This root node represents a function in the LKQL function.
 */
public final class FunctionRootNode extends MemoizedRootNode<FunctionRootNode.Arguments, Object> {

    // ----- Attributes -----

    /** Whether the function is memoized. */
    @CompilerDirectives.CompilationFinal
    private boolean isMemoized;

    // ----- Children -----

    /** The body of the function. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr body;

    private final String name;

    // ----- Constructors -----

    /**
     * Create a new function root node.
     *
     * @param language The language instance to link the root node with.
     * @param frameDescriptor The frame descriptor for the root node.
     * @param isMemoized Whether the function is memoized.
     * @param body The expression of the function.
     */
    public FunctionRootNode(
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

    // ----- Getters -----

    public Expr getBody() {
        return this.body;
    }

    // ----- Setters -----

    public void setMemoized(boolean memoized) {
        this.isMemoized = memoized;
    }

    // ----- Execution methods -----

    /**
     * @see com.oracle.truffle.api.nodes.RootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object execute(VirtualFrame frame) {
        // Initialize the frame
        this.initFrame(frame);

        // If the function is memoized, try to get the result in the cache, else
        // execute the body and set the result in the cache.
        if (this.isMemoized) {
            Arguments args = new Arguments(frame.getArguments());
            if (this.isMemoized(args)) {
                return this.getMemoized(args);
            }

            Object res = this.body.executeGeneric(frame);
            this.putMemoized(args, res);
            return res;
        }
        // Else just execute the body
        else {
            return this.body.executeGeneric(frame);
        }
    }

    // ----- Inner classes -----

    /**
     * Wraps the arguments of a function in a class for the memoization.
     *
     * @param args The arguments.
     */
    protected record Arguments(Object[] args) {
        @Override
        public boolean equals(Object o) {
            if (o == this) return true;
            if (!(o instanceof Arguments other)) return false;
            return Arrays.equals(this.args, other.args);
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(this.args);
        }
    }

    @Override
    public String getName() {
        return this.name;
    }

    @Override
    public String toString() {
        return "root::" + this.name;
    }
}
