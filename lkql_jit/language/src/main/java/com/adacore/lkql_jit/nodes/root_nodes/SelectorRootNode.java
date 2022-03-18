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

import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorArm;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorExpr;
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import com.adacore.lkql_jit.runtime.values.DepthNode;
import com.adacore.lkql_jit.runtime.values.UnitValue;

import java.util.HashMap;


/**
 * This root node represents a selector execution on a given node in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class SelectorRootNode extends BaseRootNode {

    // ----- Attributes -----

    /** The name of the selector */
    private final String name;

    /** The slot of the "this" variable */
    private final int thisSlot;

    /** The slot of the "depth" variable */
    private final int depthSlot;

    /** If the selector is memoized */
    private final boolean isMemoized;

    /** The memoization cache */
    private final HashMap<DepthNode, SelectorCallResult> memCache;

    // ----- Children -----

    /** The selector arms */
    @Children
    private final SelectorArm[] arms;

    // ----- Constructors -----

    /**
     * Create a new selector root node with the parameters
     *
     * @param language The language instance to link the root node with
     * @param frameDescriptor The frame descriptor for the root node
     * @param closure The closure of the selector
     * @param isMemoized If the selector is memoized
     * @param name The name of the selector
     * @param thisSlot The slot to put the "this" variable
     * @param depthSlot The slot to put the "depth" variable
     * @param arms The arms of the selector
     */
    public SelectorRootNode(
            TruffleLanguage<?> language,
            FrameDescriptor frameDescriptor,
            Closure closure,
            boolean isMemoized,
            String name,
            int thisSlot,
            int depthSlot,
            SelectorArm[] arms
    ) {
        super(language, frameDescriptor, closure);
        this.isMemoized = isMemoized;
        this.memCache = new HashMap<>();
        this.name = name;
        this.thisSlot = thisSlot;
        this.depthSlot = depthSlot;
        this.arms = arms;
    }

    // ----- Execution methods -----
    
    /**
     * Execute the selector on the given node, the first argument in the array
     * Return either :
     *  - A Node if the result is only a node
     *  - A Node[] if the result is an unpack of node
     * The return is wrapped in a selector call result record to have the mode information
     *
     * @see com.oracle.truffle.api.nodes.RootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object execute(VirtualFrame frame) {
        // Instantiate the closure
        this.instantiateClosure(frame);

        // Get the node and set it into the frame
        DepthNode node = (DepthNode) frame.getArguments()[0];

        // Try the memoization
        if(this.isMemoized) {
            if(this.inMemCache(node)) return this.getMemCache(node);
        }

        if(this.thisSlot > -1 && this.depthSlot > -1) {
            frame.setObject(this.thisSlot, node);
            frame.setObject(this.depthSlot, ((Integer) node.getDepth()).longValue());
        }

        // Prepare the result
        SelectorCallResult res = null;

        // Try to match an arm, if there is none, set the result to unit
        for(SelectorArm arm : this.arms) {
            res = arm.executeArm(frame, node);
            if(res != null) break;
        }
        if(res == null) {
            res = new SelectorCallResult(SelectorExpr.Mode.DEFAULT, UnitValue.getInstance());
        }

        // Do the memoization cache addition
        if(this.isMemoized) {
            this.addMemCache(node, res);
        }

        // Return the result
        return res;
    }

    // ----- Class methods -----

    /**
     * Get if the given node is in the memoization cache
     *
     * @param node The node to use as a key
     * @return True if the node is in the memoization cache, false else
     */
    @CompilerDirectives.TruffleBoundary
    private boolean inMemCache(DepthNode node) {
        return this.memCache.containsKey(node);
    }

    /**
     * Get the memoization result for the given node in the memoization cache
     *
     * @param node The node to get the result from
     * @return The result from the node in the memoization cache
     */
    @CompilerDirectives.TruffleBoundary
    private SelectorCallResult getMemCache(DepthNode node) {
        return this.memCache.get(node);
    }

    /**
     * Associate a node with a selector result in the memoization cache
     *
     * @param node The node to use as a key
     * @param res The selector call result as a value
     */
    @CompilerDirectives.TruffleBoundary
    private void addMemCache(DepthNode node, SelectorCallResult res) {
        this.memCache.put(node, res);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "Selector<" + this.name + ">";
    }

    /** @see com.oracle.truffle.api.nodes.RootNode#getSourceSection() */
    @Override
    public SourceSection getSourceSection() {
        if(this.arms[0].getLocation() != null) {
            return this.arms[0].getLocation().createSection();
        }
        return null;
    }

    // ----- Inner classes -----

    /**
     * This record represents a result of a call to the selector on a node
     *
     * @param mode The mode of the result
     * @param result The result value
     */
    public record SelectorCallResult(SelectorExpr.Mode mode, Object result) {}

}
