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

import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystem;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_classes.Iterator;
import com.adacore.lkql_jit.utils.util_functions.CheckerUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.adacore.libadalang.Libadalang;


/**
 * This class represents the "unit_checker" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class UnitCheckerFunction implements BuiltInFunction {

    // ----- Attributes -----

    /** The only instance of the "unit_checker" built-in */
    private static UnitCheckerFunction instance = null;

    /** The name of the built-in */
    public static final String NAME = "unit_checker";

    /** The expression that represents the "unit_checker" function execution */
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
        if(instance == null) {
            instance = new UnitCheckerFunction();
        }
        return instance;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getName() */
    @Override
    public String getName() {
        return NAME;
    }

    /** @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getValue() */
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

        /** The dispatcher for the rule functions */
        @Child
        @SuppressWarnings("FieldMayBeFinal")
        private FunctionDispatcher dispatcher = FunctionDispatcherNodeGen.create();
        
        /** @see com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            LKQLContext context = LKQLLanguage.getContext(this);
            Libadalang.AnalysisUnit unit;
            ObjectValue[] checkers = context.getGlobalValues().getUnitCheckers();

            try {
                unit = LKQLTypeSystemGen.expectAnalysisUnit(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ANALYSIS_UNIT,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[0]
                );
            }

            // Iterate over all checker
            for(ObjectValue rule : checkers) {
                try {
                    this.applyUnitRule(frame, rule, unit, context);
                } catch (LangkitException e) {
                    this.reportException(rule, e);
                }
            }

            // Return the unit value
            return UnitValue.getInstance();
        }

        /**
         * Apply the rule on the given unit
         *
         * @param frame The frame for the rule execution
         * @param rule The rule to execute
         * @param unit The unit to execute the rule on
         * @param context The context for the execution
         */
        private void applyUnitRule(VirtualFrame frame, ObjectValue rule, Libadalang.AnalysisUnit unit, LKQLContext context) {
            // Get the function for the checker
            FunctionValue functionValue = (FunctionValue) rule.get("function");

            // Prepare the arguments
            Object[] arguments = new Object[functionValue.getParamNames().length];
            arguments[0] = unit;
            for(int i = 1 ; i < functionValue.getDefaultValues().length ; i++) {
                arguments[i] = functionValue.getDefaultValues()[i].executeGeneric(frame);
            }

            // Put the namespace
            if(functionValue.getNamespace() != null) {
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
                if(functionValue.getNamespace() != null) {
                    context.getGlobalValues().finalizeScope();
                }
            }

            // Display all the violation message
            Iterator messageIterator = messageList.iterator();
            while(messageIterator.hasNext()) {
                ObjectValue message = (ObjectValue) messageIterator.next();
                Libadalang.AdaNode node;
                String messageText;
                try {
                    node = LKQLTypeSystemGen.expectAdaNode(message.get("loc"));
                } catch (UnexpectedResultException e) {
                    throw LKQLRuntimeException.wrongType(
                            LKQLTypesHelper.ADA_NODE,
                            LKQLTypesHelper.fromJava(e.getResult()),
                            functionValue.getBody()
                    );
                }
                try {
                    messageText = LKQLTypeSystemGen.expectString(message.get("message"));
                } catch (UnexpectedResultException e) {
                    throw LKQLRuntimeException.wrongType(
                            LKQLTypesHelper.LKQL_STRING,
                            LKQLTypesHelper.fromJava(e.getResult()),
                            functionValue.getBody()
                    );
                }
                this.reportViolation(messageText, node);
            }
        }

        /**
         * Report a rule violation with the node that violate it
         *
         * @param message The message of the violation
         * @param node The node that violated the rule
         */
        @CompilerDirectives.TruffleBoundary
        private void reportViolation(String message, Libadalang.AdaNode node) {
            if(node instanceof Libadalang.BasicDecl basicDecl) {
                CheckerUtils.printRuleViolation(message, basicDecl.pDefiningName());
            } else {
                CheckerUtils.printRuleViolation(message, node);
            }
        }

        /**
         * Report the langkit exception raised by a rule
         *
         * @param rule The rule which caused the exception
         * @param e The exception to report
         */
        @CompilerDirectives.TruffleBoundary
        private void reportException(ObjectValue rule, LangkitException e) {
            System.out.println("TODO : Report exception");
        }
        
    }

}
