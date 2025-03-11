//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.checker.built_ins;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.annotations.BuiltInFunction;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.checker.UnitChecker;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLObject;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.LalLocationWrapper;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/** This class represents the "unit_checker" built-in function in the LKQL language. */
public final class UnitCheckerFunction {

    @BuiltInFunction(name = "unit_checker", doc = "Given a unit, apply all the unit checkers on it")
    abstract static class UnitCheckerExpr extends BuiltInBody {

        private final InteropLibrary interopLibrary = InteropLibrary.getUncached();

        @Specialization
        public Object alwaysTrue(VirtualFrame frame, Libadalang.AnalysisUnit unit) {
            // Get the arguments
            LKQLContext context = LKQLLanguage.getContext(this);
            UnitChecker[] checkers = context.getUnitCheckersFiltered();

            // Iterate over all checker
            for (UnitChecker checker : checkers) {
                try {
                    this.applyUnitRule(frame, checker, unit, context);
                } catch (LangkitException e) {
                    // Report LAL exception only in debug mode
                    if (context.isCheckerDebug()) {
                        context
                            .getDiagnosticEmitter()
                            .emitDiagnostic(
                                CheckerUtils.MessageKind.ERROR,
                                e.getMsg(),
                                new LalLocationWrapper(unit.getRoot(), context.linesCache),
                                new SourceSectionWrapper(e.getLocation().getSourceSection()),
                                checker.getName()
                            );
                    }
                } catch (LKQLRuntimeException e) {
                    context
                        .getDiagnosticEmitter()
                        .emitDiagnostic(
                            CheckerUtils.MessageKind.ERROR,
                            e.getErrorMessage(),
                            new LalLocationWrapper(unit.getRoot(), context.linesCache),
                            new SourceSectionWrapper(e.getLocation().getSourceSection()),
                            checker.getName()
                        );
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
         */
        private void applyUnitRule(
            VirtualFrame frame,
            UnitChecker checker,
            Libadalang.AnalysisUnit unit,
            LKQLContext context
        ) {
            // Get the function for the checker
            final LKQLFunction functionValue = checker.getFunction();

            // Retrieve the checker name
            final String aliasName = checker.getAlias();
            final String lowerRuleName = StringUtils.toLowerCase(checker.getName());

            // Prepare the arguments
            Object[] arguments = new Object[functionValue.parameterNames.length + 1];
            arguments[1] = unit;
            for (int i = 1; i < functionValue.getParameterDefaultValues().length; i++) {
                String paramName = functionValue.parameterNames[i];
                Object userDefinedArg = context.getRuleArg(
                    (aliasName == null ? lowerRuleName : StringUtils.toLowerCase(aliasName)),
                    paramName
                );
                arguments[i + 1] = userDefinedArg == null
                    ? functionValue.getParameterDefaultValues()[i].executeGeneric(frame)
                    : userDefinedArg;
            }

            // Put the closure in the arguments
            arguments[0] = functionValue.closure.getContent();

            // Get the message list from the checker function
            final Iterable violationList;
            try {
                violationList = LKQLTypeSystemGen.expectIterable(
                    this.interopLibrary.execute(functionValue, arguments)
                );
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    functionValue.getBody()
                );
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
                        functionValue.getBody()
                    );
                }

                // Get the violation location
                Object loc = violation.getUncached("loc");
                final Libadalang.AnalysisUnit locUnit;
                final Libadalang.SourceLocationRange slocRange;
                final LangkitSupport.NodeInterface[] genericInstantiations;

                if (LKQLTypeSystemGen.isToken(loc)) {
                    final Libadalang.Token token = LKQLTypeSystemGen.asToken(loc);
                    locUnit = token.unit;
                    slocRange = token.sourceLocationRange;
                    genericInstantiations = new LangkitSupport.NodeInterface[0];
                } else if (LKQLTypeSystemGen.isNodeInterface(loc)) {
                    final LangkitSupport.NodeInterface node = LKQLTypeSystemGen.asNodeInterface(
                        loc
                    );
                    locUnit = node.getUnit();
                    slocRange = node.getSourceLocationRange();
                    genericInstantiations = node.pGenericInstantiations();
                } else {
                    throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.NODE_INTERFACE,
                        LKQLTypesHelper.fromJava(loc),
                        functionValue.getBody()
                    );
                }

                context
                    .getDiagnosticEmitter()
                    .emitRuleViolation(
                        checker,
                        message,
                        new LalLocationWrapper(slocRange, locUnit, context.linesCache),
                        genericInstantiations,
                        context
                    );
            }
        }
    }
}
