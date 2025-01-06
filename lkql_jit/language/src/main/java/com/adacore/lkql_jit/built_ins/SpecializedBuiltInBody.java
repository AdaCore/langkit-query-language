//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.LKQLTypeSystem;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;

public abstract class SpecializedBuiltInBody<
                T extends SpecializedBuiltInBody.SpecializedBuiltInNode>
        extends AbstractBuiltInFunctionBody {

    // ----- Attributes -----

    /** This node represents the execution of the built-in function. */
    @Child protected T specializedNode;

    // ----- Constructors -----

    /** Create a new specialized body with its corresponding execution node. */
    public SpecializedBuiltInBody(T specializedNode) {
        this.specializedNode = specializedNode;
        this.specializedNode.body = this;
    }

    // ----- Instance methods -----

    /** Dispatch the function arguments to the specialized execution node. */
    protected abstract Object dispatch(Object[] args);

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.dispatch(frame.getArguments());
    }

    // ----- Inner classes -----

    /** This class represents an execution node, payload of a built-in body. */
    @TypeSystemReference(LKQLTypeSystem.class)
    public abstract static class SpecializedBuiltInNode extends Node {
        // ----- Attributes -----

        /** The built-in body that owns this specialized node. */
        protected SpecializedBuiltInBody body;
    }
}
