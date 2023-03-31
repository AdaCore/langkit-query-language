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

package com.adacore.lkql_jit.nodes.declarations.functions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.declarations.DeclAnnotation;
import com.adacore.lkql_jit.nodes.declarations.Declaration;
import com.adacore.lkql_jit.nodes.expressions.FunExpr;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_functions.ArrayUtils;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a function declaration in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class FunDecl extends Declaration {

    // ----- Macros and enums -----

    /**
     * Enum representing the checker mode of a function
     */
    public enum CheckerMode {
        OFF,
        NODE,
        UNIT
    }

    /**
     * The names of the parameters for a checker annotation
     */
    private static final String[] checkerParamNames = new String[]{
        "message",
        "help",
        "follow_generic_instantiations",
        "category",
        "subcategory",
        "remediation",
        "execution_cost",
        "parametric_exemption",
        "impact",
        "target"
    };

    /**
     * The default values for annotation parameters
     */
    private static final Object[] defaultCheckerParams = new Object[]{
        null,
        null,
        false,
        "Misc",
        "Misc",
        "MEDIUM",
        0L,
        false,
        "",
        "amd64"
    };

    /**
     * The valid value for the remediation parameters
     */
    private static final String[] validRemediation = new String[]{
        "EASY",
        "MEDIUM",
        "MAJOR"
    };

    // ----- Attributes -----

    /**
     * The name of the function
     */
    protected final String name;

    /**
     * The slot to place the function in
     */
    protected final int slot;

    /**
     * If the function is declared as memoized
     */
    protected final boolean isMemoized;

    /**
     * The checker mode of the function (off, node, unit)
     */
    protected final CheckerMode checkerMode;

    // ----- Children -----

    /**
     * The expression of the function
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected FunExpr funExpr;

    // ----- Constructor -----

    /**
     * Create a new function declaration node
     *
     * @param location   The location of the node in the sources
     * @param annotation The annotation associated with the declaration
     * @param name       The name of the function
     * @param slot       The slot to place the function in
     * @param funExpr    The expression of the function
     */
    protected FunDecl(
        SourceLocation location,
        DeclAnnotation annotation,
        String name,
        int slot,
        FunExpr funExpr
    ) {
        super(location);
        this.name = name;
        this.slot = slot;
        this.annotation = annotation;
        this.funExpr = funExpr;

        // Initialize the annotation flags
        if (this.annotation != null) {
            this.isMemoized = this.annotation.getName().equals(DeclAnnotation.MEMOIZED);
            this.checkerMode = this.annotation.getName().equals(DeclAnnotation.NODE_CHECK) ?
                CheckerMode.NODE :
                this.annotation.getName().equals(DeclAnnotation.UNIT_CHECK) ?
                    CheckerMode.UNIT :
                    CheckerMode.OFF;
        } else {
            this.isMemoized = false;
            this.checkerMode = CheckerMode.OFF;
        }
    }

    // ----- Internal methods -----

    /**
     * Export the checker with the correct form in the global scope
     *
     * @param frame         The frame for the annotation execution
     * @param functionValue The function to export as a checker
     */
    protected void exportChecker(VirtualFrame frame, FunctionValue functionValue) {
        // Execute the arguments of the checker annotation
        Object[] checkerArgs = this.annotation.getArguments().executeArgList(
            frame,
            checkerParamNames
        );

        // Set the default values of the checker arguments
        for (int i = 0; i < checkerArgs.length; i++) {
            if (checkerArgs[i] == null) checkerArgs[i] = defaultCheckerParams[i];
        }

        // Verify the message and help
        if (checkerArgs[0] == null) checkerArgs[0] = functionValue.getName();
        if (checkerArgs[1] == null) checkerArgs[1] = functionValue.getName();

        // Verify the remediation mode
        if (ArrayUtils.indexOf(validRemediation, checkerArgs[5]) == -1) checkerArgs[5] = defaultCheckerParams[5];

        // Create the object value representing the checker
        ObjectValue checkerObject = new ObjectValue(
            ArrayUtils.concat(checkerParamNames, new String[]{"function", "name", "mode"}),
            ArrayUtils.concat(
                checkerArgs,
                new Object[]{
                    functionValue,
                    functionValue.getName(),
                    this.checkerMode
                }
            )
        );

        // Put the object in the context
        LKQLLanguage.getContext(this).getGlobalValues().addChecker(
            StringUtils.toLowerCase(functionValue.getName()),
            checkerObject
        );
    }

}
