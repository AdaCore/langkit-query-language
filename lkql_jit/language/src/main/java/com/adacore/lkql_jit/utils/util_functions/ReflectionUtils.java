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

import com.adacore.lkql_jit.runtime.values.NullValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerDirectives;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.utils.source_location.Locatable;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;


/**
 * Util functions to perform reflection operations and facilitate the libadalang usage
 *
 * @author Hugo GUERRIER
 */
public final class ReflectionUtils {

    /**
     * Refine the argument by translating the needed argument
     *
     * @param source The source arguments
     * @param method The method to translate for
     * @param node The node on which the method is called
     * @return The refined arguments
     */
    @CompilerDirectives.TruffleBoundary
    public static Object[] refineArguments(Object[] source, Method method, Libadalang.AdaNode node) {
        // Prepare the result and get the parameter types
        Object[] res = new Object[source.length];
        Class<?>[] types = method.getParameterTypes();

        // Iterate over source argument to transform them
        for(int i = 0 ; i < source.length ; i++) {
            Object raw = LKQLTypesHelper.fromLKQLValue(source[i]);

            // If the required parameter is a symbol
            if(types[i].equals(Libadalang.Symbol.class)) {
                try {
                    res[i] = Libadalang.Symbol.create(node.getUnit().getContext(), (String) raw);
                } catch (Libadalang.SymbolException e) {
                    throw LKQLRuntimeException.fromMessage(e.getMessage());
                }
            }

            // Else, just put the raw argument
            else {
                res[i] = raw;
            }
        }

        // Return the refined arguments
        return res;
    }

    /**
     * Call a property on a node and get the result
     *
     * @param node The node to call on
     * @param methods The methods map to perform the name call resolution
     * @param caller The caller position
     * @param argList The argument list
     * @param arguments The arguments for the call
     * @return The result of the property call
     */
    @CompilerDirectives.TruffleBoundary
    public static Object callProperty(
            Libadalang.AdaNode node,
            Map<Integer, Method> methods,
            Locatable caller,
            ArgList argList,
            Object... arguments
    ) throws LangkitException {
        Object[] refinedArgs = new Object[0];
        Method validMethod = null;
        try {
            // Verify if the method with the number of argument exists
            if(methods.containsKey(arguments.length)) {
                validMethod = methods.get(arguments.length);
                refinedArgs = ReflectionUtils.refineArguments(arguments, validMethod, node);
                return validMethod.invoke(node, refinedArgs);
            }

            // Else throw an exception
            else {
                int defaultIndex = methods.keySet().stream().max(Integer::compareTo).get();
                throw LKQLRuntimeException.wrongArity(methods.get(defaultIndex).getParameterCount(), arguments.length, argList);
            }
        } catch (IllegalArgumentException e) {
            // Explore the argument types
            for(int i = 0 ; i < refinedArgs.length ; i++) {
                if(!validMethod.getParameterTypes()[i].isInstance(refinedArgs[i])) {
                    throw LKQLRuntimeException.wrongType(
                            validMethod.getParameterTypes()[i].getSimpleName().toUpperCase(),
                            refinedArgs[i].getClass().getSimpleName().toUpperCase(),
                            argList.getArgs()[i]
                    );
                }
            }

            // Throw a default exception
            throw LKQLRuntimeException.fromMessage(
                    "Invalid argument type that cannot be annotated",
                    argList
            );
        } catch (InvocationTargetException e) {
            Throwable targetException = e.getTargetException();
            if(targetException instanceof Libadalang.LangkitException langkitException) {
                throw new LangkitException(langkitException.getKind().toString(), langkitException.getMessage());
            }
            throw new LangkitException("PropertyError", "Unhandled property error");
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
     * @param startClass The class to get the field from
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
