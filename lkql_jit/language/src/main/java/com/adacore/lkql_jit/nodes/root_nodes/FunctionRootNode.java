/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

import java.util.Arrays;


/**
 * This root node represents a function in the LKQL function.
 *
 * @author Hugo GUERRIER
 */
public final class FunctionRootNode extends MemoizedRootNode<FunctionRootNode.Arguments, Object> {

    // ----- Attributes -----

    /**
     * Whether the function is memoized.
     */
    @CompilerDirectives.CompilationFinal
    private boolean isMemoized;

    // ----- Children -----

    /**
     * The body of the function.
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr body;

    // ----- Constructors -----

    /**
     * Create a new function root node.
     *
     * @param language        The language instance to link the root node with.
     * @param frameDescriptor The frame descriptor for the root node.
     * @param isMemoized      Whether the function is memoized.
     * @param body            The expression of the function.
     */
    public FunctionRootNode(
        TruffleLanguage<?> language,
        FrameDescriptor frameDescriptor,
        boolean isMemoized,
        Expr body
    ) {
        super(language, frameDescriptor);
        this.isMemoized = isMemoized;
        this.body = body;
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

        // If the function is memoized, try to get the result in the cache, else execute the body and set the result
        // in the cache.
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
    protected record Arguments(
        Object[] args
    ) {
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

}
