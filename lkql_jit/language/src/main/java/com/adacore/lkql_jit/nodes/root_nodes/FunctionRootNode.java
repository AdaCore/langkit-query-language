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
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import java.util.Arrays;
import java.util.HashMap;


/**
 * This root node represents a function in the LKQL function
 *
 * @author Hugo GUERRIER
 */
public final class FunctionRootNode extends BaseRootNode {

    // ----- Attributes -----

    /** The name of the function */
    @CompilerDirectives.CompilationFinal
    private String name;

    /** If the function is memoized */
    @CompilerDirectives.CompilationFinal
    private boolean isMemoized;

    /** The memoization cache */
    private final HashMap<Arguments, Object> memCache;

    // ----- Children -----

    /** The body of the function */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr body;

    // ----- Constructors -----

    /**
     * Create a new function root node
     *
     * @param language The language instance to link the root node with
     * @param frameDescriptor The frame descriptor for the root node
     * @param closure The execution closure of the root node
     * @param isMemoized If the function is memoized
     * @param slots The slots of the arguments
     * @param name The name of the function
     * @param body The expression of the function
     */
    public FunctionRootNode(
            TruffleLanguage<?> language,
            FrameDescriptor frameDescriptor,
            Closure closure,
            boolean isMemoized,
            int[] slots,
            String name,
            Expr body
    ) {
        super(language, frameDescriptor, closure);
        this.argumentWriting = this.createArgWritings(slots);
        this.name = name;
        this.isMemoized = isMemoized;
        this.memCache = new HashMap<>();
        this.body = body;
    }

    // ----- Getters -----

    public Expr getBody() {
        return this.body;
    }

    // ----- Setters -----

    public void setName(String name) {
        this.name = name;
    }

    public void setMemoized(boolean memoized) {
        isMemoized = memoized;
    }

    // ----- Execution methods -----

    /** @see com.oracle.truffle.api.nodes.RootNode#execute(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object execute(VirtualFrame frame) {
        // Try the memoization
        if(this.isMemoized) {
            Arguments args = new Arguments(frame.getArguments());
            if(this.inMemCache(args)) {
                return this.getMemCache(args);
            }
        }

        // Instantiate the closure
        this.instantiateClosure(frame);

        // Instantiate the arguments
        this.instantiateArgs(frame);

        // If the function is memoized, store the result
        if(this.isMemoized) {
            Object res = this.body.executeGeneric(frame);
            Arguments args = new Arguments(frame.getArguments());
            this.addMemCache(args, res);
            return res;
        }

        // Else just execute the body
        else {
            return this.body.executeGeneric(frame);
        }
    }

    // ----- Class methods -----

    /**
     * Get if the given arguments are in the memoization cache
     *
     * @param args The arguments to use as a key
     * @return True if the arguments are in the memoization cache, false else
     */
    @CompilerDirectives.TruffleBoundary
    private boolean inMemCache(Arguments args) {
        return this.memCache.containsKey(args);
    }

    /**
     * Get the memoization result for the given arguments in the memoization cache
     *
     * @param args The arguments to get the result from
     * @return The cached function result for the given arguments
     */
    @CompilerDirectives.TruffleBoundary
    private Object getMemCache(Arguments args) {
        return this.memCache.get(args);
    }

    /**
     * Put a result of the function associated with the arguments in the memoization cache
     *
     * @param args The arguments of the function call
     * @param res The result of the function call
     */
    @CompilerDirectives.TruffleBoundary
    private void addMemCache(Arguments args, Object res) {
        this.memCache.put(args, res);
    }

    // ----- Override functions ------

    @Override
    public String toString() {
        return "Function<" + this.name + ">";
    }

    /** @see com.oracle.truffle.api.nodes.RootNode#getSourceSection() */
    @Override
    public SourceSection getSourceSection() {
        if(this.body.getLocation() != null) {
            return this.body.getLocation().createSection();
        }
        return null;
    }

    // ----- Inner classes -----

    /**
     * Wraps the arguments of a function in a class for the memoization
     *
     * @param args The arguments
     */
    private record Arguments (
            Object[] args
    ) {
        @Override
        public boolean equals(Object o) {
            if(o == this) return true;
            if(!(o instanceof Arguments other)) return false;
            return Arrays.equals(this.args, other.args);
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(this.args);
        }
    }

}
