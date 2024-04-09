//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.LKQLFunction;
import com.adacore.lkql_jit.built_ins.values.LKQLUnit;
import com.adacore.lkql_jit.checker.BaseChecker;
import com.adacore.lkql_jit.checker.NodeChecker;
import com.adacore.lkql_jit.checker.UnitChecker;
import com.adacore.lkql_jit.nodes.expressions.FunExpr;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a function declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class FunctionDeclaration extends Declaration {

    // ----- Attributes -----

    /** Name of the declared function. */
    private final String name;

    /** Local slot to place the function value in. */
    private final int slot;

    /** If the function is annotated as memoized. */
    private final boolean isMemoized;

    /** If the function is annotated as a checker. */
    private final CheckerMode checkerMode;

    // ----- Children -----

    /** Expression to get the function value from. */
    @Child private FunExpr functionExpression;

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
            final SourceLocation location,
            final Annotation annotation,
            final String name,
            final int slot,
            final FunExpr functionExpression) {
        super(location, annotation);
        this.name = name;
        this.slot = slot;
        this.functionExpression = functionExpression;

        // Initialize the annotation related fields
        if (annotation != null) {
            final String annotationName = annotation.getName();
            this.isMemoized = annotationName.equals(Constants.ANNOTATION_MEMOIZED);
            if (annotationName.equals(Constants.ANNOTATION_NODE_CHECK)) {
                this.checkerMode = CheckerMode.NODE;
            } else if (annotationName.equals(Constants.ANNOTATION_UNIT_CHECK)) {
                this.checkerMode = CheckerMode.UNIT;
            } else {
                this.checkerMode = CheckerMode.OFF;
            }
        } else {
            this.isMemoized = false;
            this.checkerMode = CheckerMode.OFF;
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

        // If the function is a checker, place it in the context
        if (this.checkerMode != CheckerMode.OFF) {
            this.exportChecker(frame, functionValue);
        }

        // Write the slot in the frame
        FrameUtils.writeLocal(frame, this.slot, functionValue);

        // Return the unit value
        return LKQLUnit.INSTANCE;
    }

    // ----- Instance methods -----

    /** Export the checker object in the LKQL context. */
    private void exportChecker(VirtualFrame frame, LKQLFunction functionValue) {
        // Execute the annotation arguments
        final Object[] checkerArguments =
                this.annotation
                        .getArguments()
                        .executeArgList(frame, Constants.CHECKER_PARAMETER_NAMES);

        // Set the default values of the checker arguments
        for (int i = 0; i < checkerArguments.length; i++) {
            if (checkerArguments[i] == null)
                checkerArguments[i] = Constants.CHECKER_PARAMETER_DEFAULT_VALUES[i];
        }

        // Verify the message and help
        if (checkerArguments[0] == null) checkerArguments[0] = this.name;
        if (checkerArguments[1] == null) checkerArguments[1] = this.name;

        // Verify the remediation mode
        final BaseChecker.Remediation remediation =
                switch ((String) checkerArguments[5]) {
                    case "EASY" -> BaseChecker.Remediation.EASY;
                    case "MAJOR" -> BaseChecker.Remediation.MAJOR;
                    default -> BaseChecker.Remediation.MEDIUM;
                };

        // Create the object value representing the checker
        final BaseChecker checker =
                this.checkerMode == CheckerMode.NODE
                        ? new NodeChecker(
                                this.name,
                                functionValue,
                                (String) checkerArguments[0],
                                (String) checkerArguments[1],
                                (boolean) checkerArguments[2],
                                (String) checkerArguments[3],
                                (String) checkerArguments[4],
                                remediation,
                                (long) checkerArguments[6],
                                (boolean) checkerArguments[7],
                                (String) checkerArguments[8],
                                (String) checkerArguments[9])
                        : new UnitChecker(
                                this.name,
                                functionValue,
                                (String) checkerArguments[0],
                                (String) checkerArguments[1],
                                (boolean) checkerArguments[2],
                                (String) checkerArguments[3],
                                (String) checkerArguments[4],
                                remediation,
                                (long) checkerArguments[6],
                                (boolean) checkerArguments[7],
                                (String) checkerArguments[8],
                                (String) checkerArguments[9]);

        // Put the object in the context
        LKQLLanguage.getContext(this)
                .getGlobal()
                .addChecker(StringUtils.toLowerCase(functionValue.getName()), checker);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"slot"}, new Object[] {this.slot});
    }

    // ----- Inner classes -----

    /** Enum representing the checker mode of a function */
    public enum CheckerMode {
        /** The function is not a checker. */
        OFF,

        /** The function is a node checker. */
        NODE,

        /** The function is a unit checker. */
        UNIT
    }
}
