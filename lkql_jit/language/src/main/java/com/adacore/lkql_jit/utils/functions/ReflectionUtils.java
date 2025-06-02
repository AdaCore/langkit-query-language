//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.exception.utils.UnsupportedTypeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

/**
 * Util functions to perform reflection operations and facilitate the libadalang usage.
 *
 * @author Hugo GUERRIER
 */
public final class ReflectionUtils {

    /**
     * Refine the LKQL argument in a langkit argument.
     *
     * @param source The source argument to refine.
     * @param type The type that the langkit library awaits.
     * @param argument The argument node (for debug purpose).
     * @return The refined argument.
     */
    public static Object refineArgument(Object source, Class<?> type, Expr argument) {
        // Get the result from the type helper
        Object res = LKQLTypesHelper.fromLKQLValue(source);

        // If the required type is a symbol
        if (type.equals(Libadalang.Symbol.class)) {
            try {
                String resString;
                try {
                    resString = LKQLTypeSystemGen.expectString(res);
                } catch (UnexpectedResultException e) {
                    throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        argument
                    );
                }
                res = Libadalang.Symbol.create(resString);
            } catch (Libadalang.SymbolException e) {
                throw LKQLRuntimeException.fromMessage(e.getMessage());
            }
        }
        // If the required type is a unit array
        // TODO: Genericise LKQL issue #501. Can't make this code work using the LangkitSupport
        //  interface (convert a LangkitSupport.AnalysisUnit array to an LKQL list.
        else if (type.equals(Libadalang.AnalysisUnit[].class)) {
            LKQLList resList;
            try {
                resList = LKQLTypeSystemGen.expectLKQLList(res);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    argument
                );
            }
            LangkitSupport.AnalysisUnit[] resArray =
                // TODO: Genericise LKQL issue. See comment above.
                new Libadalang.AnalysisUnit[(int) resList.size()];
            for (int i = 0; i < resList.size(); i++) {
                try {
                    resArray[i] = LKQLTypeSystemGen.expectAnalysisUnit(resList.get(i));
                } catch (UnexpectedResultException e) {
                    throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ANALYSIS_UNIT,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        argument
                    );
                }
            }
            res = resArray;
        }

        // Return the result
        return res;
    }

    /**
     * Call a property on a node and get the result.
     */
    @CompilerDirectives.TruffleBoundary
    public static Object callProperty(
        LangkitSupport.NodeInterface node,
        LangkitSupport.Reflection.Field fieldDescription,
        Node caller,
        Expr[] args,
        Object... arguments
    ) throws LangkitException, UnsupportedTypeException {
        // Verify if there is more arguments than params
        if (arguments.length > fieldDescription.getParams().size()) {
            throw LKQLRuntimeException.wrongArity(
                fieldDescription.getParams().size(),
                arguments.length,
                caller
            );
        }

        Object[] refinedArgs = new Object[fieldDescription.getParams().size()];
        try {
            // Create the argument list with the default values if needed
            for (int i = 0; i < refinedArgs.length; i++) {
                LangkitSupport.Reflection.Param currentParam = fieldDescription.getParams().get(i);
                if (i < arguments.length) {
                    refinedArgs[i] = refineArgument(arguments[i], currentParam.getType(), args[i]);
                } else {
                    if (currentParam.getDefaultValue().isPresent()) {
                        refinedArgs[i] = currentParam.getDefaultValue().get();
                    } else {
                        throw LKQLRuntimeException.wrongArity(
                            fieldDescription.getParams().size(),
                            arguments.length,
                            caller
                        );
                    }
                }
            }

            // Call the method
            return LKQLTypesHelper.toLKQLValue(
                fieldDescription.getJavaMethod().invoke(node, refinedArgs)
            );
        } catch (IllegalArgumentException e) {
            // Explore the argument types
            for (int i = 0; i < refinedArgs.length; i++) {
                if (!fieldDescription.getParams().get(i).getType().isInstance(refinedArgs[i])) {
                    throw LKQLRuntimeException.conversionError(
                        LKQLTypesHelper.fromJava(refinedArgs[i]),
                        LKQLTypesHelper.debugName(fieldDescription.getParams().get(i).getType()),
                        caller
                    );
                }
            }

            // Throw a default exception
            throw LKQLRuntimeException.fromMessage(
                "Invalid argument type, but cannot find which",
                caller
            );
        } catch (InvocationTargetException e) {
            Throwable targetException = e.getTargetException();
            if (targetException instanceof Libadalang.LangkitException langkitException) {
                throw new LangkitException(
                    langkitException.getMessage(),
                    caller.getSourceSection()
                );
            } else if (targetException instanceof Error error) {
                // Forward fatal errors without wrapping them in LKQLRuntimeExceptions: those
                // shouldn't
                // be caught as they imply that resuming execution is not appropriate. This could
                // for
                // example
                // be an ExitException.
                throw error;
            }
            throw LKQLRuntimeException.fromJavaException(e.getTargetException(), caller);
        } catch (IllegalAccessException e) {
            throw LKQLRuntimeException.fromMessage("Java reflection access failed", caller);
        }
    }

    /**
     * Get the class simple name of an object.
     *
     * @param obj The object to get the simple name from.
     * @return The simple name of the object class.
     */
    @CompilerDirectives.TruffleBoundary
    public static String getClassSimpleName(Object obj) {
        return obj.getClass().getSimpleName();
    }

    /**
     * Get all declared fields for a given class including the inherited ones.
     *
     * @param startClass The class to get the field from.
     * @param exclusiveParent The class to gu up maximum.
     * @return An iterable on the class fields.
     */
    @CompilerDirectives.TruffleBoundary
    public static List<Field> getFieldsUpTo(Class<?> startClass, Class<?> exclusiveParent) {
        List<Field> currentClassFields = new ArrayList<>(List.of(startClass.getDeclaredFields()));
        Class<?> parentClass = startClass.getSuperclass();

        if (parentClass != null && (!(parentClass.equals(exclusiveParent)))) {
            List<Field> parentClassFields = getFieldsUpTo(parentClass, exclusiveParent);
            currentClassFields.addAll(parentClassFields);
        }

        return currentClassFields;
    }
}
