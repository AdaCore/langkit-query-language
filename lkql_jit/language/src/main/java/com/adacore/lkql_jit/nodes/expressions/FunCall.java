//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.built_ins.BuiltInMethodValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
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
import com.adacore.lkql_jit.utils.functions.ReflectionUtils;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Arrays;

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

    public final String[] argNames;

    @Children
    public final Expr[] args;

    // ----- Constructors -----

    /**
     * Create a new function call node. Arguments are stored in two separate
     * arrays, one for the argument names, one for the argument's expressions.
     *
     * @param location The location of the node in the source.
     * @param isSafe Whether the function call is protected with a safe operator.
     */
    protected FunCall(SourceSection location, boolean isSafe, Expr[] args, String[] argNames) {
        super(location);
        this.isSafe = isSafe;
        this.args = args;
        this.argNames = argNames;
    }

    public abstract Expr getCallee();

    public Expr[] orderThisArgList(LKQLFunction function) {
        return orderArgList(
            function.parameterNames,
            argNames,
            args,
            function.getParameterDefaultValues(),
            this
        );
    }

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
        var defaultVals = method.getParameterDefaultValues();
        // Execute the argument list with the "this" value
        Expr[] argExprs = orderArgList(
            Arrays.copyOfRange(method.parameterNames, 1, method.parameterNames.length),
            argNames,
            args,
            Arrays.copyOfRange(defaultVals, 1, defaultVals.length),
            this
        );

        Object[] argVals = new Object[argExprs.length + 1];
        argVals[0] = method.thisValue;
        for (int i = 1; i < argVals.length; i++) {
            argVals[i] = argExprs[i - 1] == null ? null : argExprs[i - 1].executeGeneric(frame);
        }

        // Return the result of the method call
        return this.executeLKQLFunction(frame, method, methodLibrary, argVals);
    }

    @Specialization(
        limit = Constants.SPECIALIZED_LIB_LIMIT,
        guards = { "function.rootNode == cachedFunction.rootNode" }
    )
    @ExplodeLoop
    protected Object onCachedFunction(
        VirtualFrame frame,
        LKQLFunction function,
        @CachedLibrary("function") InteropLibrary functionLibrary,
        @Cached("function") @SuppressWarnings("unused") LKQLFunction cachedFunction,
        @Cached("function.hasClosure()") boolean hasClosure,
        @Cached("orderThisArgList(function)") Expr[] argExprs
    ) {
        // Assert that the number of arguments is constant from the POV of the
        // compiler, which will allow to unroll the loop and inline the call
        // below.
        CompilerAsserts.compilationConstant(argExprs.length);
        CompilerAsserts.compilationConstant(hasClosure);

        Object[] argVals;
        if (hasClosure) {
            argVals = new Object[argExprs.length + 1];
            argVals[0] = function.closure.getContent();
            for (int i = 1; i < argVals.length; i++) {
                argVals[i] = argExprs[i - 1].executeGeneric(frame);
            }
        } else {
            argVals = new Object[argExprs.length];
            for (int i = 0; i < argVals.length; i++) {
                argVals[i] = argExprs[i].executeGeneric(frame);
            }
        }

        try {
            return functionLibrary.execute(function, argVals);
        } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
            throw LKQLRuntimeException.fromJavaException(e, this.getCallee());
        }
    }

    @Specialization(replaces = "onCachedFunction", limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onUncachedFunction(
        VirtualFrame frame,
        LKQLFunction function,
        @CachedLibrary("function") InteropLibrary functionLibrary
    ) {
        return onCachedFunction(
            frame,
            function,
            functionLibrary,
            function,
            function.hasClosure(),
            orderThisArgList(function)
        );
    }

    /**
     * Execute the function call on a property reference value.
     */
    @Specialization
    @ExplodeLoop
    protected Object onProperty(VirtualFrame frame, LKQLProperty property) {
        CompilerAsserts.compilationConstant(this.args.length);

        // Execute the arguments as a simple array
        Object[] arguments = new Object[this.args.length];
        for (int i = 0; i < arguments.length; i++) {
            arguments[i] = this.args[i].executeGeneric(frame);
        }

        // Call the property and return its result
        try {
            return ReflectionUtils.callProperty(
                property.node,
                property.description,
                this,
                args,
                arguments
            );
        } catch (com.adacore.lkql_jit.exception.utils.UnsupportedTypeException e) {
            throw LKQLRuntimeException.unsupportedType(e.getType(), this);
        }
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
        // Verify the argument number
        if (args.length < 1) {
            throw LKQLRuntimeException.selectorWithoutNode(this);
        }

        // Return the selector list value
        return selectorValue.getList(args[0].executeGeneric(frame));
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

    @CompilerDirectives.TruffleBoundary
    public static Expr[] orderArgList(String[] paramNames, ArgList argList) {
        return FunCall.orderArgList(
            paramNames,
            Arrays.stream(argList.getArgs()).map(Arg::getArgStringName).toArray(String[]::new),
            Arrays.stream(argList.getArgs()).map(Arg::getArgExpr).toArray(Expr[]::new),
            null,
            argList
        );
    }

    public static Expr[] orderArgList(
        String[] paramNames,
        String[] argNames,
        Expr[] args,
        Expr[] defaultValues,
        LKQLNode caller
    ) {
        // Verify the arity
        if (paramNames.length < args.length) {
            throw LKQLRuntimeException.wrongArity(paramNames.length, args.length, caller);
        }

        // Prepare the result
        Expr[] res = new Expr[paramNames.length];

        // Fill the result
        for (int i = 0; i < args.length; i++) {
            // Prepare the turn
            Expr arg = args[i];
            int index = i;

            String argName = null;
            // If the current argument is a named arg
            if (argNames[i] != null) {
                argName = argNames[i];
                index = ArrayUtils.indexOf(paramNames, argName);
            }

            // There is no corresponding parameter
            if (index > -1) {
                res[index] = arg;
            } else {
                throw LKQLRuntimeException.unknownArgument(argName, arg);
            }
        }

        // Verify that all arguments are present
        for (int i = 0; i < res.length; i++) {
            if (res[i] == null) {
                // If no defaultValues array is passed, we don't try to fill it,
                // and we don't raise exceptions on missing arguments.
                if (defaultValues != null) {
                    if (defaultValues[i] != null) {
                        res[i] = defaultValues[i];
                    } else {
                        throw LKQLRuntimeException.missingArgument(i + 1, caller);
                    }
                }
            }
        }

        // Return the result array
        return res;
    }

    // ----- Instance methods -----

    /** Util function to call an LKQL function with its precomputed arguments. */
    private Object executeLKQLFunction(
        VirtualFrame frame,
        LKQLFunction function,
        InteropLibrary functionLibrary,
        Object[] args
    ) {
        // Include the closure in arguments if required
        // We don't place the closure in the arguments because built-ins don't have any.
        // Just execute the function.
        try {
            return functionLibrary.execute(function, function.computeArgs(args));
        } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
            // TODO: Implement runtime checks in the LKQLFunction class and base computing on them
            // (#138)
            throw LKQLRuntimeException.fromJavaException(e, this.getCallee());
        }
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
