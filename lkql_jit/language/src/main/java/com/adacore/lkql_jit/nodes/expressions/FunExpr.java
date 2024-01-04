//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.declarations.ParameterDeclaration;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.adacore.lkql_jit.utils.Constants;
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

    // ----- Constructors -----

    /**
     * Create a new function expression node.
     *
     * @param location The location of the node in the source.
     * @param frameDescriptor The frame descriptor for the function root node.
     * @param closureDescriptor The closure descriptor for the function.
     * @param parameters The parameters of the function.
     * @param body The body of the function.
     */
    public FunExpr(
            final SourceSection location,
            final FrameDescriptor frameDescriptor,
            final ClosureDescriptor closureDescriptor,
            final ParameterDeclaration[] parameters,
            final String documentation,
            final Expr body) {
        super(location);
        this.closureDescriptor = closureDescriptor;
        this.functionRootNode =
                new FunctionRootNode(LKQLLanguage.getLanguage(this), frameDescriptor, false, body);
        this.parameterNames = new String[parameters.length];
        this.parameterValues = new Expr[parameters.length];
        this.documentation = documentation;

        this.initParams(parameters);
    }

    /** Initialize the parameter fields */
    private void initParams(ParameterDeclaration[] parameters) {
        for (int i = 0; i < parameters.length; i++) {
            this.parameterNames[i] = parameters[i].getName();
            this.parameterValues[i] = parameters[i].getDefaultValue();
        }
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeFunction(frame);
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.expressions.Expr#executeFunction(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public LKQLFunction executeFunction(VirtualFrame frame) {
        return new LKQLFunction(
                this.functionRootNode,
                Closure.create(frame.materialize(), this.closureDescriptor),
                Constants.FUNCTION_DEFAULT_NAME,
                this.documentation,
                this.parameterNames,
                this.parameterValues);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] {"names"},
                new Object[] {Arrays.toString(this.parameterNames)});
    }
}
