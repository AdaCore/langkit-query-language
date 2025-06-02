//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.checker.BaseChecker;
import com.adacore.lkql_jit.checker.NodeChecker;
import com.adacore.lkql_jit.checker.UnitChecker;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.adacore.lkql_jit.utils.functions.OptionalUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

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
    @Child
    FunctionDeclaration functionDecl;

    // ----- Constructors -----

    public CheckerExport(
        final SourceSection location,
        final Annotation annotation,
        final CheckerMode mode,
        final FunctionDeclaration functionDecl
    ) {
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
                frame,
                (LKQLFunction) FrameUtils.readLocal(frame, this.functionDecl.slot)
            );

        return LKQLUnit.INSTANCE;
    }

    // ----- Instance methods -----

    /** Export the given LKQL function */
    private void exportChecker(VirtualFrame frame, LKQLFunction functionValue) {
        final Expr[] orderedArguments = FunCall.orderArgList(
            Constants.CHECKER_PARAMETER_NAMES,
            this.annotation.getArguments()
        );
        // Execute the annotation arguments
        Object[] checkerArguments = new Object[orderedArguments.length];
        for (int i = 0; i < checkerArguments.length; i++) {
            checkerArguments[i] = orderedArguments[i] != null
                ? orderedArguments[i].executeGeneric(frame)
                : null;
        }

        // Set the default values of the checker arguments
        for (int i = 0; i < checkerArguments.length; i++) {
            if (checkerArguments[i] == null) checkerArguments[i] =
                Constants.CHECKER_PARAMETER_DEFAULT_VALUES[i];
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

        // Get the auto fix function
        final var autoFixObject = checkerArguments[10];
        final var autoFixArg = this.annotation.getArguments().getArgWithName("auto_fix");

        // If there is an auto fix, the checker must be a node checker
        if (autoFixObject != null && this.mode == CheckerMode.UNIT) {
            throw LKQLRuntimeException.fromMessage(
                "Auto fixes not available for unit checks",
                autoFixArg.orElseGet(() -> this.annotation.getArguments().getArgs()[10])
            );
        }

        // Check that the auto fix object is a function
        if (autoFixObject != null && !LKQLTypeSystemGen.isLKQLFunction(autoFixObject)) {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_FUNCTION,
                LKQLTypesHelper.fromJava(autoFixObject),
                OptionalUtils.map(autoFixArg, a -> (LKQLNode) a.getArgExpr()).orElseGet(() ->
                    this.annotation.getArguments().getArgs()[10]
                )
            );
        }
        final var autoFix = autoFixObject == null
            ? null
            : LKQLTypeSystemGen.asLKQLFunction(autoFixObject);

        // Create the object value representing the checker
        final BaseChecker checker = this.mode == CheckerMode.NODE
            ? new NodeChecker(
                functionValue.name,
                functionValue,
                autoFix,
                (String) checkerArguments[0],
                (String) checkerArguments[1],
                (boolean) checkerArguments[2],
                (String) checkerArguments[3],
                (String) checkerArguments[4],
                remediation,
                (long) checkerArguments[6],
                (boolean) checkerArguments[7],
                (String) checkerArguments[8]
            )
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
                (String) checkerArguments[8]
            );

        // Put the object in the context
        LKQLLanguage.getContext(this)
            .getGlobal()
            .addChecker(StringUtils.toLowerCase(functionValue.getName()), checker);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "mode" },
                new Object[] { this.mode }
            );
    }

    // ----- Inner classes -----

    /** Enum representing the mode of a checker function. */
    public enum CheckerMode {
        /** The function is a node checker. */
        NODE,

        /** The function is a unit checker. */
        UNIT,
    }
}
