//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltInMethodValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLProperty;
import com.adacore.lkql_jit.runtime.values.LKQLSelector;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.runtime.values.interfaces.Nullish;
import com.adacore.lkql_jit.runtime.values.lists.LKQLSelectorList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.source.SourceSection;

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
     * @param argList The arguments of the function call.
     */
    protected FunCall(SourceSection location, boolean isSafe, ArgList argList) {
        super(location);
        this.isSafe = isSafe;
        this.argList = argList;
    }

    // ----- Getters -----

    public ArgList getArgList() {
        return argList;
    }

    public abstract Expr getCallee();

    // ----- Execution methods -----

    /**
     * Execute the function call node when the callee is a built-in method value.
     *
     * @return The result of the method call.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onBuiltinMethod(
        VirtualFrame frame,
        BuiltInMethodValue method,
        @CachedLibrary("method") InteropLibrary methodLibrary
    ) {
        method.setCallNode(this);

        // Execute the argument list with the "this" value
        String[] actualParam = method.parameterNames;
        Object[] args = this.argList.executeArgList(frame, actualParam, 1);
        args[0] = method.thisValue;

        // Return the result of the method call
        return this.executeLKQLFunction(frame, method, methodLibrary, args, false);
    }

    /**
     * Execute the function call on a built-in function.
     *
     * @param frame The frame to execute the built-in in.
     * @param function The built-in function.
     * @return The result of the built-in call.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onBuiltinFunction(
        VirtualFrame frame,
        BuiltInFunctionValue function,
        @CachedLibrary("function") InteropLibrary functionLibrary
    ) {
        function.setCallNode(this);

        // Execute the argument list
        String[] actualParam = function.parameterNames;
        Object[] args = this.argList.executeArgList(frame, actualParam);

        // Execute the built-in function value
        return this.executeLKQLFunction(frame, function, functionLibrary, args, false);
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
        @CachedLibrary("function") InteropLibrary functionLibrary
    ) {
        // Execute the argument list with the mandatory space for the function closure
        String[] actualParam = function.parameterNames;
        Object[] args = this.argList.executeArgList(frame, actualParam);

        // Return the result of the function execution
        return executeLKQLFunction(frame, function, functionLibrary, args, true);
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

        // Return the selector list value
        return selectorValue.getList(argList[0].getArgExpr().executeGeneric(frame));
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
            this
        );
    }

    // ----- Instance methods -----

    /** Util function to call an LKQL function with its precomputed arguments. */
    private Object executeLKQLFunction(
        VirtualFrame frame,
        LKQLFunction function,
        InteropLibrary functionLibrary,
        Object[] args,
        boolean includeClosure
    ) {
        // Verify that all arguments are present
        Expr[] defaultValues = function.getParameterDefaultValues();
        for (int i = 0; i < args.length; i++) {
            if (args[i] == null) {
                if (defaultValues[i] != null) {
                    args[i] = defaultValues[i].executeGeneric(frame);
                } else {
                    throw LKQLRuntimeException.missingArgument(i + 1, this);
                }
            }
        }

        // Include the closure in arguments if required
        if (includeClosure) {
            args = ArrayUtils.concat(new Object[] { function.closure.getContent() }, args);
        }

        // We don't place the closure in the arguments because built-ins don't have any.
        // Just execute the function.
        try {
            pushCallStack(function);
            return functionLibrary.execute(function, args);
        } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
            // TODO: Implement runtime checks in the LKQLFunction class and base computing on them
            // (#138)
            throw LKQLRuntimeException.fromJavaException(e, this.getCallee());
        } finally {
            popCallStack();
        }
    }

    /** Push this call node to the call stack. */
    private void pushCallStack(LKQLFunction function) {
        LKQLLanguage.getContext(this).callStack.pushCall(function, this);
    }

    /** Remove this call node from the call stack. */
    private void popCallStack() {
        LKQLLanguage.getContext(this).callStack.popCall();
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "isSafe" },
                new Object[] { this.isSafe }
            );
    }
}
