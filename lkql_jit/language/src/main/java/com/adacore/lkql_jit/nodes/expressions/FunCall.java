//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.values.*;
import com.adacore.lkql_jit.built_ins.values.interfaces.Nullish;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLSelectorList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
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
     * @param builtIn The built-in function.
     * @return The result of the built-in call.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onBuiltIn(
            VirtualFrame frame,
            BuiltInFunctionValue builtIn,
            @CachedLibrary("builtIn") InteropLibrary builtInLibrary) {
        // Set the call node in the built-in function
        builtIn.setCallNode(this);

        // Get the real argument names and default values
        String[] actualParam = builtIn.parameterNames;
        Expr[] defaultValues = builtIn.parameterDefaultValues;

        // Execute the argument list
        // TODO: Do not materialize the frame here, for now we need to do it because of a Truffle
        // compilation error
        Object[] realArgs =
                this.argList.executeArgList(
                        frame.materialize(), actualParam, builtIn.getThisValue() == null ? 0 : 1);

        // Add the "this" value to the arguments
        if (builtIn.getThisValue() != null) {
            realArgs[0] = builtIn.getThisValue();
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
        try {
            return builtInLibrary.execute(builtIn, realArgs);
        } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
            // TODO: Implement runtime checks in the LKQLFunction class and base computing on them
            // (#138)
            throw LKQLRuntimeException.fromJavaException(e, this.calleeLocation);
        }
    }

    /**
     * Execute the function call on a function value.
     *
     * @param frame The frame to execution the function in.
     * @param function The function value to execute.
     * @return The result of the function call.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onFunction(
            final VirtualFrame frame,
            final LKQLFunction function,
            @CachedLibrary("function") InteropLibrary functionLibrary) {
        // Get the real argument names and default values
        String[] names = function.parameterNames;
        Expr[] defaultValues = function.parameterDefaultValues;

        // Prepare the argument array and the working var
        // TODO: Do not materialize the frame here, for now we need to do it because of a Truffle
        // compilation error
        Object[] realArgs = this.argList.executeArgList(frame.materialize(), names);

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
        realArgs = ArrayUtils.concat(new Object[] {function.closure.getContent()}, realArgs);

        // Return the result of the function call
        try {
            return functionLibrary.execute(function, realArgs);
        } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
            // TODO: Implement runtime checks in the LKQLFunction class and base computing on them
            // (#138)
            throw LKQLRuntimeException.fromJavaException(e, this.calleeLocation);
        }
    }

    /**
     * Execute the function call on a property reference value.
     *
     * @param frame The frame to execute the property reference in.
     * @param property The property reference value to execute.
     * @return The result of the property call.
     */
    @Specialization
    protected Object onProperty(VirtualFrame frame, LKQLProperty property) {
        // Execute the arguments as a simple array
        Object[] arguments = this.argList.executeArgList(frame);

        // Call the property and return its result
        return property.executeAsProperty(this, this.argList, arguments);
    }

    /**
     * Execute function call on a selector value.
     *
     * @param frame The frame to execute the selector value in.
     * @param selectorValue The selector value to execute.
     * @return The result of the selector value execution.
     */
    @Specialization
    protected LKQLSelectorList onSelector(VirtualFrame frame, LKQLSelector selectorValue) {
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
        return selectorValue.getList(node);
    }

    /**
     * If the function call is executed on a nullish value and is safe.
     *
     * @param value The value that is not used.
     * @return The unit value.
     */
    @Specialization(guards = "isSafe")
    protected Object onNullish(@SuppressWarnings("unused") final Nullish value) {
        return LKQLUnit.INSTANCE;
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
