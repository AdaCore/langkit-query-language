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

package com.adacore.lkql_jit.utils.util_functions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.exception.utils.UnsupportedTypeException;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.Locatable;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;


/**
 * Util functions to perform reflection operations and facilitate the libadalang usage
 *
 * @author Hugo GUERRIER
 */
public final class ReflectionUtils {

    /**
     * Refine the LKQL argument in a langkit argument
     *
     * @param source   The source argument to refine
     * @param type     The type that the langkit library awaits
     * @param argument The argument node (for debug purpose)
     * @return The refined argument
     */
    public static Object refineArgument(Object source, Class<?> type, Arg argument) {
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
        else if (type.equals(Libadalang.AnalysisUnitArray.class)) {
            ListValue resList;
            try {
                resList = LKQLTypeSystemGen.expectListValue(res);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    argument
                );
            }
            Libadalang.AnalysisUnitArray resArray = Libadalang.AnalysisUnitArray.create((int) resList.size());
            for (int i = 0; i < resList.size(); i++) {
                try {
                    resArray.set(i, LKQLTypeSystemGen.expectAnalysisUnit(resList.get(i)));
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
     * Call a property on a node and get the result
     *
     * @param node             The node to call on
     * @param fieldDescription The description of the field to execute.
     * @param caller           The caller position
     * @param argList          The argument list
     * @param arguments        The arguments for the call
     * @return The result of the property call
     */
    @CompilerDirectives.TruffleBoundary
    public static Object callProperty(
        Libadalang.AdaNode node,
        Libadalang.LibadalangField fieldDescription,
        Locatable caller,
        ArgList argList,
        Object... arguments
    ) throws LangkitException, UnsupportedTypeException {
        // Verify if there is more arguments than params
        if (arguments.length > fieldDescription.params.size()) {
            throw LKQLRuntimeException.wrongArity(fieldDescription.params.size(), arguments.length, argList);
        }

        Object[] refinedArgs = new Object[fieldDescription.params.size()];
        try {
            // Create the argument list with the default values if needed
            for (int i = 0; i < refinedArgs.length; i++) {
                Libadalang.Param currentParam = fieldDescription.params.get(i);
                if (i < arguments.length) {
                    refinedArgs[i] = refineArgument(
                        arguments[i],
                        currentParam.type,
                        argList.getArgs()[i]
                    );
                } else {
                    if (currentParam instanceof Libadalang.ParamWithDefaultValue paramWithDefaultValue) {
                        refinedArgs[i] = paramWithDefaultValue.defaultValue;
                    } else {
                        throw LKQLRuntimeException.wrongArity(fieldDescription.params.size(), arguments.length, argList);
                    }
                }
            }

            // Call the method
            return LKQLTypesHelper.toLKQLValue(
                fieldDescription.javaMethod.invoke(node, refinedArgs)
            );
        } catch (IllegalArgumentException e) {
            // Explore the argument types
            for (int i = 0; i < refinedArgs.length; i++) {
                if (!fieldDescription.params.get(i).type.isInstance(refinedArgs[i])) {
                    throw LKQLRuntimeException.conversionError(
                        LKQLTypesHelper.fromJava(refinedArgs[i]),
                        LKQLTypesHelper.debugName(fieldDescription.params.get(i).type),
                        caller
                    );
                }
            }

            // Throw a default exception
            throw LKQLRuntimeException.fromMessage(
                "Invalid argument type, but cannot find which",
                argList
            );
        } catch (InvocationTargetException e) {
            Throwable targetException = e.getTargetException();
            if (targetException instanceof Libadalang.LangkitException langkitException) {
                throw new LangkitException(langkitException.kind.toString(), langkitException.getMessage());
            } else if (targetException instanceof Error error) {
                // Forward fatal errors without wrapping them in LKQLRuntimeExceptions: those shouldn't
                // be caught as they imply that resuming execution is not appropriate. This could for example
                // be an ExitException.
                throw error;
            }
            throw LKQLRuntimeException.fromJavaException(e.getTargetException(), caller);
        } catch (IllegalAccessException e) {
            throw LKQLRuntimeException.fromMessage("Java reflection access failed", caller);
        }
    }

    /**
     * Get the class simple name of an object
     *
     * @param obj The object to get the simple name from
     * @return The simple name of the object class
     */
    @CompilerDirectives.TruffleBoundary
    public static String getClassSimpleName(Object obj) {
        return obj.getClass().getSimpleName();
    }

    /**
     * Get all declared fields for a given class including the inherited ones
     *
     * @param startClass      The class to get the field from
     * @param exclusiveParent The class to gu up maximum
     * @return An iterable on the class fields
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
