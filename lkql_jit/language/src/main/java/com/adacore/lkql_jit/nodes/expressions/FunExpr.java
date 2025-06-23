//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.nodes.utils.CreateClosureNode;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Arrays;

/**
 * This node represents a function expression in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class FunExpr extends Expr {

    // ----- Attributes -----

    /** Closure descriptor of the function. */
    private final ClosureDescriptor closureDescriptor;

    /** The root node representing the function execution. */
    private final FunctionRootNode functionRootNode;

    /** The names of the parameters. */
    private final String[] parameterNames;

    /** Documentation for the function */
    private final String documentation;

    /** The default values of the parameters. */
    private final Expr[] parameterValues;

    public final String name;

    @Child
    CreateClosureNode createClosureNode;

    // ----- Constructors -----

    /**
     * Create a new function expression node.
     */
    public FunExpr(
        SourceSection location,
        FrameDescriptor frameDescriptor,
        ClosureDescriptor closureDescriptor,
        String[] parameterNames,
        Expr[] parameterDefaultValues,
        String documentation,
        Expr body,
        String name
    ) {
        super(location);
        this.closureDescriptor = closureDescriptor;
        this.functionRootNode = new FunctionRootNode(
            LKQLLanguage.getLanguage(this),
            frameDescriptor,
            false,
            body,
            name
        );
        this.name = name;
        this.parameterNames = parameterNames;
        this.parameterValues = parameterDefaultValues;
        this.documentation = documentation;
        this.createClosureNode = new CreateClosureNode(closureDescriptor);
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeFunction(frame);
    }

    @Override
    public LKQLFunction executeFunction(VirtualFrame frame) {
        return new LKQLFunction(
            this.functionRootNode,
            createClosureNode.execute(frame),
            name,
            this.documentation,
            this.parameterNames,
            this.parameterValues
        );
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "names" },
                new Object[] { Arrays.toString(this.parameterNames) }
            );
    }
}
