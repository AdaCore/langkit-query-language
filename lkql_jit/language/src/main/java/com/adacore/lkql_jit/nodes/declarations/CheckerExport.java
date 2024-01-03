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
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.checkers.BaseChecker;
import com.adacore.lkql_jit.utils.checkers.NodeChecker;
import com.adacore.lkql_jit.utils.checkers.UnitChecker;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents a checker exportation in the LKQL language. A function annotated with
 * `@check` or `@unit_check` is exported as a checker in the current LKQL context.
 */
public class CheckerExport extends Declaration {

    // ----- Attributes -----

    /** The Mode of the checker ("node" or "unit"). */
    public final CheckerMode mode;

    // ----- Children -----

    /** The annotated function to export as a checker. */
    @Child FunctionDeclaration functionDecl;

    // ----- Constructors -----

    public CheckerExport(
            final SourceLocation location,
            final Annotation annotation,
            final CheckerMode mode,
            final FunctionDeclaration functionDecl) {
        super(location, annotation);
        this.mode = mode;
        this.functionDecl = functionDecl;
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Execute the function declaration
        this.functionDecl.executeGeneric(frame);

        // Export the checker in the current context
        this.exportChecker(
                frame, (LKQLFunction) FrameUtils.readLocal(frame, this.functionDecl.slot));

        return LKQLUnit.INSTANCE;
    }

    // ----- Instance methods -----

    /** Export the given LKQL function */
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
        if (checkerArguments[0] == null) checkerArguments[0] = functionValue.name;
        if (checkerArguments[1] == null) checkerArguments[1] = functionValue.name;

        // Verify the remediation mode
        final BaseChecker.Remediation remediation =
                switch ((String) checkerArguments[5]) {
                    case "EASY" -> BaseChecker.Remediation.EASY;
                    case "MAJOR" -> BaseChecker.Remediation.MAJOR;
                    default -> BaseChecker.Remediation.MEDIUM;
                };

        // Create the object value representing the checker
        final BaseChecker checker =
                this.mode == CheckerMode.NODE
                        ? new NodeChecker(
                                functionValue.name,
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
                                functionValue.name,
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

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"mode"}, new Object[] {this.mode});
    }

    // ----- Inner classes -----

    /** Enum representing the mode of a checker function. */
    public enum CheckerMode {
        /** The function is a node checker. */
        NODE,

        /** The function is a unit checker. */
        UNIT
    }
}
