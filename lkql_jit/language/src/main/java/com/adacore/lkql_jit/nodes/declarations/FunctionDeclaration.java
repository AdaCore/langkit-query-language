//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.nodes.expressions.FunExpr;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a function declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class FunctionDeclaration extends Declaration {

    // ----- Attributes -----

    /** Name of the declared function. */
    public final String name;

    /** Local slot to place the function value in. */
    public final int slot;

    /** If the function is annotated as memoized. */
    public final boolean isMemoized;

    // ----- Children -----

    /** Expression to get the function value from. */
    @Child
    private FunExpr functionExpression;

    // ----- Constructors -----

    /**
     * Create a new function declaration node.
     *
     * @param location The location of the node in the source.
     * @param annotation The annotation on the function if there is one (can be null).
     * @param name The name of the declared function.
     * @param slot The local slot to place the function in.
     * @param functionExpression The expression which returns the function value.
     */
    public FunctionDeclaration(
        final SourceSection location,
        final Annotation annotation,
        final String name,
        final int slot,
        final FunExpr functionExpression
    ) {
        super(location, annotation);
        this.name = name;
        this.slot = slot;
        this.functionExpression = functionExpression;

        // Initialize the annotation related fields
        if (annotation != null) {
            final String annotationName = annotation.getName();
            this.isMemoized = annotationName.equals(Constants.ANNOTATION_MEMOIZED);
        } else {
            this.isMemoized = false;
        }
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Execute the function expression to get the functional value
        final LKQLFunction functionValue = this.functionExpression.executeFunction(frame);
        functionValue.setName(this.name);
        functionValue.rootNode.setMemoized(this.isMemoized);

        // Write the slot in the frame
        FrameUtils.writeLocal(frame, this.slot, functionValue);

        // Return the unit value
        return LKQLUnit.INSTANCE;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "slot" },
                new Object[] { this.slot }
            );
    }
}
