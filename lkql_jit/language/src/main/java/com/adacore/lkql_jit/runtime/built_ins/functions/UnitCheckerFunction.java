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

package com.adacore.lkql_jit.runtime.built_ins.functions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_classes.Iterator;
import com.adacore.lkql_jit.utils.util_functions.CheckerUtils;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This class represents the "unit_checker" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class UnitCheckerFunction implements BuiltInFunction {

    // ----- Attributes -----

    /**
     * The only instance of the "unit_checker" built-in
     */
    private static UnitCheckerFunction instance = null;

    /**
     * The name of the built-in
     */
    public static final String NAME = "unit_checker";

    /**
     * The expression that represents the "unit_checker" function execution
     */
    private final UnitCheckerFunction.UnitCheckerExpr unitCheckerExpr;

    // ----- Constructors -----

    /**
     * This private constructor
     */
    private UnitCheckerFunction() {
        this.unitCheckerExpr = new UnitCheckerExpr();
    }

    /**
     * Get the only instance of the built-in
     *
     * @return The built-in instance
     */
    public static UnitCheckerFunction getInstance() {
        if (instance == null) {
            instance = new UnitCheckerFunction();
        }
        return instance;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getName()
     */
    @Override
    public String getName() {
        return NAME;
    }

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getValue()
     */
    @Override
    public BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
            NAME,
            "Given a unit, apply all the unit checker on it",
            new String[]{"unit"},
            new Expr[]{null},
            this.unitCheckerExpr
        );
    }

    // ----- Inner classes -----

    /**
     * This class is the expression of the "unit_checker" function
     */
    private static final class UnitCheckerExpr extends BuiltInExpr {

        /**
         * The dispatcher for the rule functions
         */
        @Child
        @SuppressWarnings("FieldMayBeFinal")
        private FunctionDispatcher dispatcher = FunctionDispatcherNodeGen.create();

        /**
         * @see com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
         */
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            LKQLContext context = LKQLLanguage.getContext(this);
            Libadalang.AnalysisUnit unit;
            ObjectValue[] checkers = context.getUnitCheckersFiltered();

            try {
                unit = LKQLTypeSystemGen.expectAnalysisUnit(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.ANALYSIS_UNIT,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.callNode.getArgList().getArgs()[0]
                );
            }

            // Initialize the cache that will contain decoded source lines of all needed units
            CheckerUtils.SourceLinesCache linesCache = new CheckerUtils.SourceLinesCache();

            // Iterate over all checker
            for (ObjectValue rule : checkers) {
                try {
                    this.applyUnitRule(frame, rule, unit, context, linesCache);
                } catch (LangkitException e) {
                    reportException(context, rule, e);
                } catch (LKQLRuntimeException e) {
                    reportException(context, e);
                }
            }

            // Return the unit value
            return UnitValue.getInstance();
        }

        /**
         * Apply the rule on the given unit
         *
         * @param frame      The frame for the rule execution
         * @param rule       The rule to execute
         * @param unit       The unit to execute the rule on
         * @param context    The context for the execution
         * @param linesCache The cache of all units' source text lines
         */
        private void applyUnitRule(
            VirtualFrame frame,
            ObjectValue rule,
            Libadalang.AnalysisUnit unit,
            LKQLContext context,
            CheckerUtils.SourceLinesCache linesCache
        ) {
            // Get the function for the checker
            final FunctionValue functionValue = (FunctionValue) rule.get("function");

            // Retrieve the rule name
            final String ruleName = (String) rule.get("name");

            // Prepare the arguments
            Object[] arguments = new Object[functionValue.getParamNames().length];
            arguments[0] = unit;
            for (int i = 1; i < functionValue.getDefaultValues().length; i++) {
                String paramName = functionValue.getParamNames()[i];
                Object userDefinedArg = context.getRuleArg((String) rule.get("name"), paramName);
                arguments[i] = userDefinedArg == null ?
                    functionValue.getDefaultValues()[i].executeGeneric(frame) :
                    userDefinedArg;
            }

            // Put the namespace
            if (functionValue.getNamespace() != null) {
                context.getGlobalValues().pushNamespace(functionValue.getNamespace());
            }

            // Get the message list from the rule function
            Iterable messageList;
            try {
                messageList = LKQLTypeSystemGen.expectIterable(this.dispatcher.executeDispatch(functionValue, arguments));
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    functionValue.getBody()
                );
            } finally {
                // Remove the namespace
                if (functionValue.getNamespace() != null) {
                    context.getGlobalValues().popNamespace();
                }
            }

            // Display all the violation message
            Iterator messageIterator = messageList.iterator();
            while (messageIterator.hasNext()) {
                ObjectValue message = (ObjectValue) messageIterator.next();

                // Get the message text
                String messageText;
                try {
                    messageText = LKQLTypeSystemGen.expectString(message.get("message"));
                } catch (UnexpectedResultException e) {
                    throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        functionValue.getBody()
                    );
                }

                // Get the message location
                Object loc = message.get("loc");
                final Libadalang.AnalysisUnit locUnit;
                final Libadalang.SourceLocationRange slocRange;
                final Libadalang.AdaNodeArray genericInstantiations;

                if (LKQLTypeSystemGen.isToken(loc)) {
                    final Libadalang.Token token = LKQLTypeSystemGen.asToken(loc);
                    locUnit = token.unit;
                    slocRange = token.sourceLocationRange;
                    genericInstantiations = Libadalang.AdaNodeArray.NONE;
                } else if (LKQLTypeSystemGen.isAdaNode(loc)) {
                    final Libadalang.AdaNode node = LKQLTypeSystemGen.asAdaNode(loc);
                    locUnit = node.getUnit();
                    slocRange = node.getSourceLocationRange();
                    genericInstantiations = node.pGenericInstantiations();
                } else {
                    throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ADA_NODE,
                        LKQLTypesHelper.fromJava(loc),
                        functionValue.getBody()
                    );
                }

                context.getDiagnosticEmitter().emit(
                    ruleName,
                    messageText,
                    slocRange,
                    locUnit,
                    genericInstantiations,
                    linesCache,
                    context
                );
            }
        }

        /**
         * Report the langkit exception raised by a rule
         *
         * @param rule The rule which caused the exception
         * @param e    The exception to report
         */
        @CompilerDirectives.TruffleBoundary
        private static void reportException(LKQLContext context, ObjectValue rule, LangkitException e) {
            context.println("TODO : Report exception");
        }

        /**
         * Report the LQKL exception
         *
         * @param e The LKQL exception
         */
        @CompilerDirectives.TruffleBoundary
        private static void reportException(LKQLContext context, LKQLRuntimeException e) {
            context.println("Exception in the LKQL code :");
            context.println(e.getMessage());
        }

    }

}
