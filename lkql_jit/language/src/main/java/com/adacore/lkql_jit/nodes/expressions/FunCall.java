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

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcherNodeGen;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.*;
import com.adacore.lkql_jit.runtime.values.interfaces.Nullish;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

/**
 * This node represents a function call node in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "callee", type = Expr.class)
public abstract class FunCall extends Expr {

    // ----- Attributes -----

    /** Whether the function call is safe access. */
    protected final boolean isSafe;

    /** The location of the callee token. */
    protected final DummyLocation calleeLocation;

    // ----- Children -----

    /** The function call arguments. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ArgList argList;

    /** The function dispatch node to optimize execution. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private FunctionDispatcher dispatcher;

    // ----- Constructors -----

    /**
     * Create a new function call node.
     *
     * @param location The location of the node in the source.
     * @param isSafe Whether the function call is protected with a safe operator.
     * @param calleeLocation The location of the callee expression.
     * @param argList The arguments of the function call.
     */
    protected FunCall(
            SourceLocation location,
            boolean isSafe,
            DummyLocation calleeLocation,
            ArgList argList) {
        super(location);
        this.isSafe = isSafe;
        this.calleeLocation = calleeLocation;
        this.argList = argList;
        this.dispatcher = FunctionDispatcherNodeGen.create();
    }

    // ----- Getters -----

    public ArgList getArgList() {
        return argList;
    }

    // ----- Execute methods -----

    /**
     * Execute the function call on a built-in function.
     *
     * @param frame The frame to execute the built-in in.
     * @param builtInFunctionValue The built-in function.
     * @return The result of the built-in call.
     */
    @Specialization
    protected Object onBuiltIn(VirtualFrame frame, BuiltInFunctionValue builtInFunctionValue) {
        // Set the call node in the built-in function
        builtInFunctionValue.setCallNode(this);

        // Get the real argument names and default values
        String[] actualParam = builtInFunctionValue.getParamNames();
        Expr[] defaultValues = builtInFunctionValue.getDefaultValues();

        // Execute the argument list
        // TODO: Do not materialize the frame here, for now we need to do it because of a Truffle
        // compilation error
        Object[] realArgs =
                this.argList.executeArgList(
                        frame.materialize(),
                        actualParam,
                        builtInFunctionValue.getThisValue() == null ? 0 : 1);

        // Add the "this" value to the arguments
        if (builtInFunctionValue.getThisValue() != null) {
            realArgs[0] = builtInFunctionValue.getThisValue();
        }

        // Verify that there is all arguments
        for (int i = 0; i < realArgs.length; i++) {
            if (realArgs[i] == null) {
                if (defaultValues[i] != null) {
                    realArgs[i] = defaultValues[i].executeGeneric(frame);
                } else {
                    throw LKQLRuntimeException.missingArgument(i + 1, this);
                }
            }
        }

        // We don't place the closure in the arguments because built-ins don't have any.
        // Just execute the function.
        return this.dispatcher.executeDispatch(builtInFunctionValue, realArgs);
    }

    /**
     * Execute the function call on a function value.
     *
     * @param frame The frame to execution the function in.
     * @param functionValue The function value to execute.
     * @return The result of the function call.
     */
    @Specialization
    protected Object onFunction(VirtualFrame frame, FunctionValue functionValue) {
        // Get the real argument names and default values
        String[] actualParam = functionValue.getParamNames();
        Expr[] defaultValues = functionValue.getDefaultValues();

        // Prepare the argument array and the working var
        // TODO: Do not materialize the frame here, for now we need to do it because of a Truffle
        // compilation error
        Object[] realArgs = this.argList.executeArgList(frame.materialize(), actualParam);

        // Verify if there is no missing argument and evaluate the default values
        for (int i = 0; i < realArgs.length; i++) {
            if (realArgs[i] == null) {
                if (defaultValues[i] != null) {
                    realArgs[i] = defaultValues[i].executeGeneric(frame);
                } else {
                    throw LKQLRuntimeException.missingArgument(i + 1, this);
                }
            }
        }

        // Place the closure in the arguments
        realArgs =
                ArrayUtils.concat(new Object[] {functionValue.getClosure().getContent()}, realArgs);

        // Return the result of the function call
        return this.dispatcher.executeDispatch(functionValue, realArgs);
    }

    /**
     * Execute the function call on a property reference value.
     *
     * @param frame The frame to execute the property reference in.
     * @param propertyRefValue The property reference value to execute.
     * @return The result of the property call.
     */
    @Specialization
    protected Object onProperty(VirtualFrame frame, PropertyRefValue propertyRefValue) {
        // Execute the arguments as a simple array
        Object[] arguments = this.argList.executeArgList(frame);

        // Call the property and return its result
        return propertyRefValue.execute(this, this.argList, arguments);
    }

    /**
     * Execute function call on a selector value.
     *
     * @param frame The frame to execute the selector value in.
     * @param selectorValue The selector value to execute.
     * @return The result of the selector value execution.
     */
    @Specialization
    protected SelectorListValue onSelector(VirtualFrame frame, SelectorValue selectorValue) {
        // Get the argument list and get the node from it
        Arg[] argList = this.argList.getArgs();

        // Verify the argument number
        if (argList.length < 1) {
            throw LKQLRuntimeException.selectorWithoutNode(this);
        }

        // Get the node from the argument
        Libadalang.AdaNode node;
        try {
            node = argList[0].getArgExpr().executeNode(frame);
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.ADA_NODE, LKQLTypesHelper.fromJava(e.getResult()), argList[0]);
        }

        // Return the selector list value
        return selectorValue.execute(node);
    }

    /**
     * If the function call is executed on a nullish value and is safe.
     *
     * @param value The value that is not used.
     * @return The unit value.
     */
    @Specialization(guards = "isSafe")
    protected Object onNullish(@SuppressWarnings("unused") Nullish value) {
        return UnitValue.getInstance();
    }

    /**
     * If the function call is done on a non-executable value.
     *
     * @param nonExec The non-executable value.
     */
    @Fallback
    protected void nonExecutable(Object nonExec) {
        throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_FUNCTION,
                LKQLTypesHelper.fromJava(nonExec),
                this.calleeLocation);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"isSafe"}, new Object[] {this.isSafe});
    }
}
