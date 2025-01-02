//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.utils.UnsupportedTypeException;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLObject;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.oracle.truffle.api.CompilerDirectives;
import java.math.BigInteger;

/**
 * This class contains all helper values to manipulate LKQL types.
 *
 * @author Hugo GUERRIER
 */
public final class LKQLTypesHelper {

    // ----- Static values -----

    /**
     * The string representing the LKQL iterable interface. NOTE: unlike every other Str constant in
     * this file, this is not a real type, but rather a class of types. As such this is only used in
     * error messages
     */
    public static final String LKQL_ITERABLE = "Iterable";

    /** The string representing the LKQL unit type. */
    public static final String LKQL_UNIT = "Unit";

    /** The string representing the LKQL boolean type. */
    public static final String LKQL_BOOLEAN = "Bool";

    /** The string representing the LKQL integer type. */
    public static final String LKQL_INTEGER = "Int";

    /** The string representing the LKQL string type. */
    public static final String LKQL_STRING = "Str";

    /** The string representing the LKQL pattern type. */
    public static final String LKQL_PATTERN = "Pattern";

    /** The string representing the LKQL functional type. */
    public static final String LKQL_FUNCTION = "Function";

    /** The string representing the LKQL property reference type. */
    public static final String LKQL_PROPERTY_REF = "PropertyReference";

    /** The string representing the LKQL selector type. */
    public static final String LKQL_SELECTOR = "Selector";

    /** The string representing the LKQL tuple type. */
    public static final String LKQL_TUPLE = "Tuple";

    /** The string representing the LKQL list type. */
    public static final String LKQL_LIST = "List";

    /** The string representing the LKQL lazy list type. */
    public static final String LKQL_LAZY_LIST = "LazyList";

    /** The string representing the LKQL selector list type. */
    public static final String LKQL_SELECTOR_LIST = "SelectorList";

    /** The string representing the LKQL object type. */
    public static final String LKQL_OBJECT = "Object";

    /** The string representing the LKQL namespace type. */
    public static final String LKQL_NAMESPACE = "Namespace";

    public static final String LKQL_REC_VALUE = "RecValue";

    /** The string representing the ada node type. */
    public static final String ADA_NODE = "Node";

    /** The string representing the token type. */
    public static final String TOKEN = "Token";

    /** The string representing the analysis unit type. */
    public static final String ANALYSIS_UNIT = "AnalysisUnit";

    /** The string representing the rewriting context type. */
    public static final String REWRITING_CONTEXT = "RewritingContext";

    /** The string representing the rewriting node type. */
    public static final String REWRITING_NODE = "RewritingNode";

    /** The string representing a reference to a lal member. */
    public static final String MEMBER_REFERENCE = "MemberReference";

    /**
     * Set of all LKQL types. This is necessary because we don't have a closed set of all types as
     * an enum or something
     */
    public static final String[] ALL_TYPES = {
        LKQL_UNIT,
        LKQL_BOOLEAN,
        LKQL_INTEGER,
        LKQL_STRING,
        LKQL_PATTERN,
        LKQL_FUNCTION,
        LKQL_PROPERTY_REF,
        LKQL_SELECTOR,
        LKQL_TUPLE,
        LKQL_LIST,
        LKQL_LAZY_LIST,
        LKQL_SELECTOR_LIST,
        LKQL_OBJECT,
        LKQL_NAMESPACE,
        LKQL_REC_VALUE,
        ADA_NODE,
        TOKEN,
        ANALYSIS_UNIT,
        REWRITING_CONTEXT,
        REWRITING_NODE,
        MEMBER_REFERENCE
    };

    // ----- Class methods -----

    /**
     * Create a type union from the given atomic types.
     *
     * @param types The types to union.
     * @return The string representing the union.
     */
    @CompilerDirectives.TruffleBoundary
    public static String typeUnion(String... types) {
        return String.join(" | ", types);
    }

    /** Get list type representation with its precise element type. */
    public static String listOf(String elementType) {
        return LKQL_LIST + "[" + elementType + "]";
    }

    /**
     * Get the LKQL type in a string from the Java type.
     *
     * @param obj The java object to get the LKQL type from.
     * @return The LKQL type in a string.
     */
    public static String fromJava(Object obj) {
        return fromJava(obj, "unbound_java_type");
    }

    /**
     * Get the string representation of the Java type for the LKQL language.
     *
     * @param obj The object to get the type for.
     * @param defaultValue The default value to return.
     * @return The string representing the type in the LKQL language.
     */
    public static String fromJava(Object obj, String defaultValue) {
        if (LKQLTypeSystemGen.isUnit(obj)) {
            return LKQL_UNIT;
        } else if (LKQLTypeSystemGen.isLong(obj)
                || LKQLTypeSystemGen.isBigInteger(obj)
                || obj instanceof Integer) {
            return LKQL_INTEGER;
        } else if (LKQLTypeSystemGen.isString(obj)) {
            return LKQL_STRING;
        } else if (LKQLTypeSystemGen.isLKQLPattern(obj)) {
            return LKQL_PATTERN;
        } else if (LKQLTypeSystemGen.isLKQLFunction(obj)) {
            return LKQL_FUNCTION;
        } else if (LKQLTypeSystemGen.isLKQLProperty(obj)) {
            return LKQL_PROPERTY_REF;
        } else if (LKQLTypeSystemGen.isLKQLSelector(obj)) {
            return LKQL_SELECTOR;
        } else if (LKQLTypeSystemGen.isLKQLTuple(obj)) {
            return LKQL_TUPLE;
        } else if (LKQLTypeSystemGen.isLKQLList(obj)) {
            return LKQL_LIST;
        } else if (LKQLTypeSystemGen.isLKQLLazyList(obj)) {
            return LKQL_LAZY_LIST;
        } else if (LKQLTypeSystemGen.isAdaNode(obj)) {
            return ADA_NODE;
        } else if (LKQLTypeSystemGen.isToken(obj)) {
            return TOKEN;
        } else if (LKQLTypeSystemGen.isAnalysisUnit(obj)) {
            return ANALYSIS_UNIT;
        } else if (LKQLTypeSystemGen.isBoolean(obj)) {
            return LKQL_BOOLEAN;
        } else if (LKQLTypeSystemGen.isLKQLObject(obj)) {
            return LKQL_OBJECT;
        } else if (LKQLTypeSystemGen.isLKQLNamespace(obj)) {
            return LKQL_NAMESPACE;
        } else if (LKQLTypeSystemGen.isRewritingContext(obj)) {
            return REWRITING_CONTEXT;
        } else if (LKQLTypeSystemGen.isRewritingNode(obj)) {
            return REWRITING_NODE;
        } else {
            return defaultValue;
        }
    }

    /**
     * Get the debug name for the given Java type.
     *
     * @param type The type to get the debug name for.
     * @return The type debug name.
     */
    public static String debugName(Class<?> type) {
        if (type == Boolean.class || type == boolean.class) {
            return "Bool";
        } else {
            return type.getSimpleName();
        }
    }

    /**
     * Get the category of value for the given Java type. This category represents a kind of value
     * from Libadalang which cannot be imported in the LKQL context.
     *
     * @param type The type to get the category for.
     * @return The type category.
     */
    @CompilerDirectives.TruffleBoundary
    public static String category(Class<?> type) {
        if (type == Libadalang.CompletionItemIterator.class) {
            return "ITERATOR_CATEGORY";
        } else {
            return type.getSimpleName();
        }
    }

    /**
     * Convert a Java value into a LKQL value.
     *
     * @param javaValue The Java object.
     * @return The LKQL value.
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
            return adaNode.isNone() ? LKQLNull.INSTANCE : adaNode;
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
        else if (javaValue instanceof Object[] array) {
            Object[] res = new Object[array.length];
            int i = 0;
            for (Object obj : array) {
                res[i] = toLKQLValue(obj);
                i++;
            }
            return new LKQLList(res);
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
                aspect.node.isNone() ? LKQLNull.INSTANCE : aspect.node,
                aspect.value.isNone() ? LKQLNull.INSTANCE : aspect.value
            };
            return LKQLObject.createUncached(keys, values);
        }

        // If the source is a reference result structure
        else if (javaValue instanceof Libadalang.RefResult refResultStruct) {
            String[] keys = {"kind", "ref"};
            Object[] values = {
                toLKQLValue(refResultStruct.kind),
                refResultStruct.ref.isNone() ? LKQLNull.INSTANCE : refResultStruct.ref
            };
            return LKQLObject.createUncached(keys, values);
        }

        // If the source is a parameter-actual structure
        else if (javaValue instanceof Libadalang.ParamActual paramActual) {
            String[] keys = {"actual", "param"};
            Object[] values = {paramActual.actual, paramActual.param};
            return LKQLObject.createUncached(keys, values);
        }

        // Else, throw an exception for the unsupported type
        else {
            if (javaValue == null) {
                throw new UnsupportedTypeException(Void.class);
            } else {
                throw new UnsupportedTypeException(javaValue.getClass());
            }
        }
    }

    /**
     * Get a java value from an LKQL value.
     *
     * @param lkqlValue The lkql source value.
     * @return The java value.
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
