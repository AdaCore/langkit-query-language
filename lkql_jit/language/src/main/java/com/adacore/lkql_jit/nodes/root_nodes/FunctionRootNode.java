//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.values.interop.LKQLNoValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import java.util.Arrays;

/**
 * This root node represents a function in the LKQL function.
 */
public final class FunctionRootNode extends MemoizedRootNode<FunctionRootNode.Arguments, Object> {

    // ----- Attributes -----

    /** Whether the function is memoized. */
    @CompilerDirectives.CompilationFinal
    private boolean isMemoized;

    /**
     * Whether this function takes a closure argument when it is called. This closure appears as the
     * first argument of the argument array.
     */
    @CompilerDirectives.CompilationFinal
    public final boolean takesClosure;

    public final String name;

    /** Name of the function parameters. */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final String[] parameterNames;

    // ----- Children -----

    /** Default values for the function parameters. */
    @Children
    private Expr[] defaultParameters;

    /** The body of the function. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr body;

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
        boolean takesClosure,
        String[] parameterNames,
        Expr[] defaultParameters,
        Expr body,
        String name
    ) {
        super(language, frameDescriptor);
        this.isMemoized = isMemoized;
        this.takesClosure = takesClosure;
        this.parameterNames = parameterNames;
        this.defaultParameters = defaultParameters;
        this.body = body;
        this.name = name;
    }

    // ----- Getters -----

    public String[] getParameterNames() {
        return parameterNames;
    }

    public Node[] getDefaultParameters() {
        return this.defaultParameters;
    }

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

        // Check that all arguments have a value
        this.checkParamValues(frame);

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

    /** Helper function to evaluate default parameters. */
    @ExplodeLoop
    private void checkParamValues(VirtualFrame frame) {
        // Check that all arguments have a value
        var closureOffset = this.takesClosure ? 1 : 0;
        for (int i = 0; i < this.defaultParameters.length; i++) {
            var arg = frame.getArguments()[i + closureOffset];
            if (arg == null || arg == LKQLNoValue.INSTANCE) {
                if (defaultParameters[i] == null) {
                    throw LKQLRuntimeException.missingArgument(i, parameterNames[i]);
                } else {
                    frame.getArguments()[i + closureOffset] = defaultParameters[i].executeGeneric(
                        frame
                    );
                }
            }
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
        // If the function's body is a built-in, then there is no LKQL source location to refer to
        var pfx = (this.body instanceof BuiltInBody
                ? "<builtin>"
                : this.body.getLocation().fileName() + ":" + this.body.getLocation().startLine());

        return pfx + "::" + this.name;
    }
}
