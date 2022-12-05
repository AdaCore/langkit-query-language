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

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;

import java.util.Arrays;


/**
 * This root node represents a list comprehension execution (expression and predicate) in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class ListComprehensionRootNode extends RootNode {

    // ----- Attributes -----

    /** The closure for the root node execution */
    private Closure closure;

    // ----- Children -----

    /** The binding writing nodes */
    @Children
    private final WriteArg[] bindingWriting;

    /** The predicate of the list comprehension */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr predicate;

    /** The result expression of the list comprehension */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr result;

    // ----- Constructors -----

    /**
     * Create a new list comprehension root node
     *
     * @param language The language instance to link the root node with
     * @param frameDescriptor The frame descriptor for the root node
     * @param slots The slots to put the list comprehension bindings in
     * @param predicate The predicate of the list comprehension
     * @param result The result expression of the list comprehension
     */
    public ListComprehensionRootNode(
            TruffleLanguage<?> language,
            FrameDescriptor frameDescriptor,
            int[] slots,
            Expr predicate,
            Expr result
    ) {
        super(language, frameDescriptor);
        this.bindingWriting = this.createBindingWritings(slots);
        this.predicate = predicate;
        this.result = result;
    }

    // ----- Setters -----

    public void setClosure(Closure closure) {
        this.closure = closure;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.root_nodes.BaseRootNode#execute(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object execute(VirtualFrame frame) {
        // Instantiate the closure
        this.instantiateClosure(frame);

        // Instantiate the arguments
        this.instantiateArgs(frame);

        // Evaluate the predicate and return the result if true, else just return null
        try {
            if(this.predicate == null || this.predicate.executeBoolean(frame)) {
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

    // ----- Internal methods -----

    /**
     * Create the binding writing from the list comprehension node
     *
     * @param slots The list comprehension binding writing nodes
     * @return The binding writing nodes
     */
    private WriteArg[] createBindingWritings(int[] slots) {
        // Create the result
        WriteArg[] res = new WriteArg[slots.length];

        // Create the writing nodes
        for(int i = 0 ; i < slots.length ; i++) {
            res[i] = new WriteArg(slots[i]);
        }

        // Return the result
        return res;
    }

    /**
     * Instantiate the closure if it's not null
     *
     * @param frame The frame to instantiate the closure in
     */
    private void instantiateClosure(VirtualFrame frame) {
        if(this.closure != null) {
            this.closure.instantiate(frame.materialize());
        }
    }

    /**
     * Instantiate the arguments in the local frame
     *
     * @param frame The frame to instantiate the arguments in
     */
    private void instantiateArgs(VirtualFrame frame) {
        for(int i = 0; i < this.bindingWriting.length ; i++) {
            this.bindingWriting[i].executeWrite(frame, i);
        }
    }

    // ----- Class methods -----

    /**
     * Get the call target as a generic Truffle calling interface
     *
     * @return The call target
     */
    public CallTarget getRealCallTarget() {
        return this.getCallTarget();
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "ListComprehension";
    }

    /** @see com.oracle.truffle.api.nodes.RootNode#getSourceSection() */
    @Override
    public SourceSection getSourceSection() {
        return this.result.getLocation().createSection();
    }

}
