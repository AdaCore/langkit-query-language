//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltinFunctionBody;
import com.adacore.lkql_jit.built_ins.values.LKQLFunction;
import com.adacore.lkql_jit.built_ins.values.LKQLObject;
import com.adacore.lkql_jit.built_ins.values.LKQLUnit;
import com.adacore.lkql_jit.built_ins.values.interfaces.Iterable;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.checkers.UnitChecker;
import com.adacore.lkql_jit.utils.functions.CheckerUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This class represents the "unit_checker" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class UnitCheckerFunction {

    // ----- Attributes -----

    /** The name of the built-in. */
    public static final String NAME = "unit_checker";

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a unit, apply all the unit checker on it",
                new String[] {"unit"},
                new Expr[] {null},
                new UnitCheckerExpr());
    }

    // ----- Inner classes -----

    /** This class is the expression of the "unit_checker" function. */
    private static final class UnitCheckerExpr extends BuiltinFunctionBody {

        /** An uncached interop library for the checker functions execution. */
        private InteropLibrary interopLibrary = InteropLibrary.getUncached();

        /**
         * @see BuiltinFunctionBody#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
         */
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            LKQLContext context = LKQLLanguage.getContext(this);
            Libadalang.AnalysisUnit unit;
            UnitChecker[] checkers = context.getUnitCheckersFiltered();

            try {
                unit = LKQLTypeSystemGen.expectAnalysisUnit(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ANALYSIS_UNIT,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[0]);
            }

            // Initialize the cache that will contain decoded source lines of all needed units
            CheckerUtils.SourceLinesCache linesCache = new CheckerUtils.SourceLinesCache();

            // Iterate over all checker
            for (UnitChecker checker : checkers) {
                try {
                    this.applyUnitRule(frame, checker, unit, context, linesCache);
                } catch (LangkitException e) {
                    // Report LAL exception only in debug mode
                    if (context.isCheckerDebug()) {
                        context.getDiagnosticEmitter()
                                .emitInternalError(
                                        checker.getName(),
                                        unit,
                                        Libadalang.SourceLocation.create(1, (short) 1),
                                        e.getLoc().toString(),
                                        e.getMsg(),
                                        context);
                    }
                } catch (LKQLRuntimeException e) {
                    context.getDiagnosticEmitter()
                            .emitInternalError(
                                    checker.getName(),
                                    unit,
                                    Libadalang.SourceLocation.create(1, (short) 1),
                                    e.getLocationString(),
                                    e.getRawMessage(),
                                    context);
                }
            }

            // Return the unit value
            return LKQLUnit.INSTANCE;
        }

        /**
         * Apply the checker on the given unit.
         *
         * @param frame The frame for the checker execution.
         * @param checker The checker to execute.
         * @param unit The unit to execute the checker on.
         * @param context The context for the execution.
         * @param linesCache The cache of all units' source text lines.
         */
        private void applyUnitRule(
                VirtualFrame frame,
                UnitChecker checker,
                Libadalang.AnalysisUnit unit,
                LKQLContext context,
                CheckerUtils.SourceLinesCache linesCache) {
            // Get the function for the checker
            final LKQLFunction functionValue = checker.getFunction();

            // Retrieve the checker name
            final String aliasName = checker.getAlias();
            final String lowerRuleName = StringUtils.toLowerCase(checker.getName());

            // Prepare the arguments
            Object[] arguments = new Object[functionValue.parameterNames.length + 1];
            arguments[1] = unit;
            for (int i = 1; i < functionValue.parameterDefaultValues.length; i++) {
                String paramName = functionValue.parameterNames[i];
                Object userDefinedArg =
                        context.getRuleArg(
                                (aliasName == null
                                        ? lowerRuleName
                                        : StringUtils.toLowerCase(aliasName)),
                                paramName);
                arguments[i + 1] =
                        userDefinedArg == null
                                ? functionValue.parameterDefaultValues[i].executeGeneric(frame)
                                : userDefinedArg;
            }

            // Put the closure in the arguments
            arguments[0] = functionValue.closure.getContent();

            // Get the message list from the checker function
            final Iterable violationList;
            try {
                violationList =
                        LKQLTypeSystemGen.expectIterable(
                                this.interopLibrary.execute(functionValue, arguments));
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_LIST,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        functionValue.getBody());
            } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
                // TODO: Move function runtime verification to the LKQLFunction class (#138)
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }

            // Display all the violation message
            Iterator violationIterator = violationList.iterator();
            while (violationIterator.hasNext()) {
                LKQLObject violation = (LKQLObject) violationIterator.next();

                // Get the violation text
                String message;
                try {
                    message = LKQLTypeSystemGen.expectString(violation.getUncached("message"));
                } catch (UnexpectedResultException e) {
                    throw LKQLRuntimeException.wrongType(
                            LKQLTypesHelper.LKQL_STRING,
                            LKQLTypesHelper.fromJava(e.getResult()),
                            functionValue.getBody());
                }

                // Get the violation location
                Object loc = violation.getUncached("loc");
                final Libadalang.AnalysisUnit locUnit;
                final Libadalang.SourceLocationRange slocRange;
                final Libadalang.AdaNode[] genericInstantiations;

                if (LKQLTypeSystemGen.isToken(loc)) {
                    final Libadalang.Token token = LKQLTypeSystemGen.asToken(loc);
                    locUnit = token.unit;
                    slocRange = token.sourceLocationRange;
                    genericInstantiations = new Libadalang.AdaNode[0];
                } else if (LKQLTypeSystemGen.isAdaNode(loc)) {
                    final Libadalang.AdaNode node = LKQLTypeSystemGen.asAdaNode(loc);
                    locUnit = node.getUnit();
                    slocRange = node.getSourceLocationRange();
                    genericInstantiations = node.pGenericInstantiations();
                } else {
                    throw LKQLRuntimeException.wrongType(
                            LKQLTypesHelper.ADA_NODE,
                            LKQLTypesHelper.fromJava(loc),
                            functionValue.getBody());
                }

                context.getDiagnosticEmitter()
                        .emitRuleViolation(
                                lowerRuleName,
                                message,
                                slocRange,
                                locUnit,
                                genericInstantiations,
                                linesCache,
                                context);
            }
        }
    }
}
