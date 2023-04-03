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

package com.adacore.lkql_jit.utils;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.utils.UnsupportedTypeException;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.runtime.values.NodeNull;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.oracle.truffle.api.CompilerDirectives;

import java.math.BigInteger;


/**
 * This class contains all helper values to manipulate LKQL types
 *
 * @author Hugo GUERRIER
 */
public final class LKQLTypesHelper {

    // ----- Macros and enum -----

    /**
     * The string representing the LKQL indexable interface
     */
    public static final String LKQL_INDEXABLE = "Indexable";

    /**
     * The string representing the LKQL iterable interface
     */
    public static final String LKQL_ITERABLE = "Iterable";

    /**
     * The string representing the LKQL unit type
     */
    public static final String LKQL_UNIT = "Unit";

    /**
     * The string representing the LKQL boolean type
     */
    public static final String LKQL_BOOLEAN = "Bool";

    /**
     * The string representing the LKQL integer type
     */
    public static final String LKQL_INTEGER = "Int";

    /**
     * The string representing the LKQL string type
     */
    public static final String LKQL_STRING = "Str";

    /**
     * The string representing the LKQL pattern type
     */
    public static final String LKQL_PATTERN = "Pattern";

    /**
     * The string representing the LKQL functional type
     */
    public static final String LKQL_FUNCTION = "Function";

    /**
     * The string representing the LKQL property reference type
     */
    public static final String LKQL_PROPERTY_REF = "Property_Reference";

    /**
     * The string representing the LKQL selector type
     */
    public static final String LKQL_SELECTOR = "Selector";

    /**
     * The string representing the LKQL tuple type
     */
    public static final String LKQL_TUPLE = "Tuple";

    /**
     * The string representing the LKQL list type
     */
    public static final String LKQL_LIST = "List";

    /**
     * The string representing the LKQL lazy list type
     */
    public static final String LKQL_LAZY_LIST = "Lazy_List";

    /**
     * The string representing the LKQL selector list type
     */
    public static final String LKQL_SELECTOR_LIST = "Selector_List";

    /**
     * The string representing the LKQL object type
     */
    public static final String LKQL_OBJECT = "Object";

    /**
     * The string representing the LKQL namespace type
     */
    public static final String LKQL_NAMESPACE = "Namespace";

    /**
     * The string representing the ada node type
     */
    public static final String ADA_NODE = "Node";

    /**
     * The string representing the token type
     */
    public static final String TOKEN = "Token";

    /**
     * The string representing the analysis unit type
     */
    public static final String ANALYSIS_UNIT = "Analysis_Unit";

    // ----- Class methods -----

    /**
     * Get the LKQL type in a string from the Java type
     *
     * @param obj The java object to get the LKQL type from
     * @return The LKQL type in a string
     */
    public static String fromJava(Object obj) {
        return fromJava(obj, "unbound_java_type");
    }

    /**
     * Get the string representation of the Java type for the LKQL language
     *
     * @param obj          The object to get the type for
     * @param defaultValue The default value to return
     * @return The string representing the type in the LKQL language
     */
    public static String fromJava(Object obj, String defaultValue) {
        if (LKQLTypeSystemGen.isUnit(obj)) {
            return LKQL_UNIT;
        } else if (LKQLTypeSystemGen.isLong(obj) || LKQLTypeSystemGen.isBigInteger(obj) || obj instanceof Integer) {
            return LKQL_INTEGER;
        } else if (LKQLTypeSystemGen.isString(obj)) {
            return LKQL_STRING;
        } else if (LKQLTypeSystemGen.isPattern(obj)) {
            return LKQL_PATTERN;
        } else if (LKQLTypeSystemGen.isFunctionValue(obj)) {
            return LKQL_FUNCTION;
        } else if (LKQLTypeSystemGen.isPropertyRefValue(obj)) {
            return LKQL_PROPERTY_REF;
        } else if (LKQLTypeSystemGen.isSelectorValue(obj)) {
            return LKQL_SELECTOR;
        } else if (LKQLTypeSystemGen.isTupleValue(obj)) {
            return LKQL_TUPLE;
        } else if (LKQLTypeSystemGen.isListValue(obj)) {
            return LKQL_LIST;
        } else if (LKQLTypeSystemGen.isLazyListValue(obj)) {
            return LKQL_LAZY_LIST;
        } else if (LKQLTypeSystemGen.isSelectorListValue(obj)) {
            return LKQL_SELECTOR_LIST;
        } else if (LKQLTypeSystemGen.isAdaNode(obj)) {
            return ADA_NODE;
        } else if (LKQLTypeSystemGen.isToken(obj)) {
            return TOKEN;
        } else if (LKQLTypeSystemGen.isAnalysisUnit(obj)) {
            return ANALYSIS_UNIT;
        } else if (LKQLTypeSystemGen.isBoolean(obj)) {
            return LKQL_BOOLEAN;
        } else if (LKQLTypeSystemGen.isObjectValue(obj)) {
            return LKQL_OBJECT;
        } else if (LKQLTypeSystemGen.isNamespaceValue(obj)) {
            return LKQL_NAMESPACE;
        } else {
            return defaultValue;
        }
    }

    /**
     * Get the debug name for the given Java type
     *
     * @param type The type to get the debug name for
     * @return The type debug name
     */
    public static String debugName(Class<?> type) {
        if (type == Boolean.class || type == boolean.class) {
            return "Bool";
        } else {
            return type.getSimpleName();
        }
    }

    /**
     * Convert a Java value into a LKQL value
     *
     * @param javaValue The Java object
     * @return The LKQL value
     */
    @CompilerDirectives.TruffleBoundary
    public static Object toLKQLValue(Object javaValue) throws UnsupportedTypeException {
        // If the source is a boolean
        if (javaValue instanceof Boolean bool) {
            return bool;
        }

        // If the source is an integer
        else if (javaValue instanceof Integer integer) {
            return integer.longValue();
        }

        // If the source is a long
        else if (javaValue instanceof Long) {
            return javaValue;
        }

        // If the value is a big integer
        else if (javaValue instanceof BigInteger) {
            return javaValue;
        }

        // If the value is a character
        else if (javaValue instanceof Libadalang.Char character) {
            return character.toString();
        }

        // If the source is a symbol
        else if (javaValue instanceof Libadalang.Symbol symbol) {
            return symbol.text;
        }

        // If the value is a string
        else if (javaValue instanceof String) {
            return javaValue;
        }

        // If the source is an AdaNode
        else if (javaValue instanceof Libadalang.AdaNode adaNode) {
            return adaNode.isNone() ? NodeNull.getInstance() : adaNode;
        }

        // If the source is a token
        else if (javaValue instanceof Libadalang.Token token) {
            return token;
        }

        // If the source is an analysis unit
        else if (javaValue instanceof Libadalang.AnalysisUnit) {
            return javaValue;
        }

        // If the source is an array from libadalang
        else if (javaValue instanceof Libadalang.ArrayBase<?> array) {
            Object[] res = new Object[array.size()];
            int i = 0;
            for (Object obj : array) {
                res[i] = toLKQLValue(obj);
                i++;
            }
            return new ListValue(res);
        }

        // If the source is an enum
        else if (javaValue instanceof Enum<?>) {
            return javaValue.toString().toLowerCase();
        }

        // If the source is an aspect structure
        else if (javaValue instanceof Libadalang.Aspect aspect) {
            String[] keys = {"exists", "inherited", "node", "value"};
            Object[] values = {
                aspect.exists,
                aspect.inherited,
                aspect.node.node.isNull() ?
                    NodeNull.getInstance() :
                    Libadalang.AdaNode.fromEntity(aspect.node),
                aspect.value.node.isNull() ?
                    NodeNull.getInstance() :
                    Libadalang.AdaNode.fromEntity(aspect.value)
            };
            return new ObjectValue(keys, values);
        }

        // If the source is a reference result structure
        else if (javaValue instanceof Libadalang.RefResult refResultStruct) {
            String[] keys = {"kind", "ref"};
            Object[] values = {
                toLKQLValue(refResultStruct.kind),
                refResultStruct.ref.node.isNull() ?
                    NodeNull.getInstance() :
                    Libadalang.AdaNode.fromEntity(refResultStruct.ref)
            };
            return new ObjectValue(keys, values);
        }

        // If the source is a parameter-actual structure
        else if (javaValue instanceof Libadalang.ParamActual paramActual) {
            String[] keys = {"actual", "param"};
            Object[] values = {
                Libadalang.AdaNode.fromEntity(paramActual.actual),
                Libadalang.AdaNode.fromEntity(paramActual.param)
            };
            return new ObjectValue(keys, values);
        }

        // Else, throw an exception for the unsupported type
        else {
            if (javaValue == null) {
                throw new UnsupportedTypeException("NULL");
            } else {
                throw new UnsupportedTypeException(javaValue.getClass().getSimpleName());
            }
        }
    }

    /**
     * Get a java value from an LKQL value
     *
     * @param lkqlValue The lkql source value
     * @return The java value
     */
    public static Object fromLKQLValue(Object lkqlValue) {
        // If the source is nullish
        if (LKQLTypeSystemGen.isNullish(lkqlValue)) {
            return null;
        }

        // If the source is a long
        else if (LKQLTypeSystemGen.isLong(lkqlValue)) {
            return (int) LKQLTypeSystemGen.asLong(lkqlValue);
        }

        // If the source is a big integer
        else if (LKQLTypeSystemGen.isBigInteger(lkqlValue)) {
            return LKQLTypeSystemGen.asBigInteger(lkqlValue).intValue();
        }

        // Just return the value
        else {
            return lkqlValue;
        }
    }

}
