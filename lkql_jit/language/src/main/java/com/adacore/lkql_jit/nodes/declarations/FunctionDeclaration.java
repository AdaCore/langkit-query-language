/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.FunExpr;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
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
        final FunctionValue functionValue = this.functionExpression.executeFunction(frame);
        functionValue.setName(this.name);
        functionValue.setMemoized(this.isMemoized);

        // If the function is a checker, place it in the context
        if (this.checkerMode != CheckerMode.OFF) {
            this.exportChecker(frame, functionValue);
        }

        // Write the slot in the frame
        FrameUtils.writeLocal(frame, this.slot, functionValue);

        // Return the unit value
        return UnitValue.getInstance();
    }

    // ----- Instance methods -----

    /** Export the checker object in the LKQL context. */
    private void exportChecker(VirtualFrame frame, FunctionValue functionValue) {
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
        if (ArrayUtils.indexOf(Constants.CHECKER_VALID_REMEDIATION, checkerArguments[5]) == -1) {
            checkerArguments[5] = Constants.CHECKER_PARAMETER_DEFAULT_VALUES[5];
        }

        // Create the object value representing the checker
        final ObjectValue checkerObject =
                new ObjectValue(
                        ArrayUtils.concat(
                                Constants.CHECKER_PARAMETER_NAMES,
                                new String[] {"function", "name", "mode"}),
                        ArrayUtils.concat(
                                checkerArguments,
                                new Object[] {functionValue, this.name, this.checkerMode}));

        // Put the object in the context
        LKQLLanguage.getContext(this)
                .getGlobal()
                .addChecker(StringUtils.toLowerCase(functionValue.getName()), checkerObject);
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
