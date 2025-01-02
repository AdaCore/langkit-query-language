//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents all type of arguments in the LKQL language. */
public abstract class Arg extends LKQLNode {

    // ----- Attributes -----

    /** The argument name, can be null if the arg is an expr one. */
    protected final Identifier argName;

    // ----- Children -----

    /** The expression of the argument. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr argExpr;

    // ----- Constructors -----

    /**
     * Create a new argument node.
     *
     * @param location The location of the argument in the sources.
     * @param argName The name of the argument.
     * @param argExpr The expression of the argument.
     */
    protected Arg(SourceSection location, Identifier argName, Expr argExpr) {
        super(location);
        this.argName = argName;
        this.argExpr = argExpr;
    }

    // ----- Getters -----

    public Identifier getArgName() {
        return this.argName;
    }

    public Expr getArgExpr() {
        return this.argExpr;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.argExpr.executeGeneric(frame);
    }
}
