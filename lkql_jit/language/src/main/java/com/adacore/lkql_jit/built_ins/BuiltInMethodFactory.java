//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import java.util.Map;

/**
 * This class is a factory for a specific built-in method. This factory will build a new method
 * value each time the method is instantiated.
 */
public final class BuiltInMethodFactory {

    // ----- Attributes -----

    public final String name;

    public final String documentation;

    public final String[] paramNames;

    public final Expr[] paramDefaults;

    public final BuiltInBody methodBody;

    /**
     * Whether the factory should produce "attribute" value. See the {@link BuiltInAttributeValue}
     * class for more information.
     */
    public final boolean isAttribute;

    // ----- Constructors -----

    /** Create a new method factory directly with the method body. */
    public BuiltInMethodFactory(
            String name,
            String documentation,
            String[] paramNames,
            Expr[] paramDefaults,
            BuiltInBody methodBody,
            boolean isAttribute) {
        this.name = name;
        this.documentation = documentation;
        this.paramNames = paramNames;
        this.paramDefaults = paramDefaults;
        this.methodBody = methodBody;
        this.isAttribute = isAttribute;
    }

    // ----- Instance methods -----

    /** Instantiate the method with the given "thisValue" and return the LKQL value. */
    public BuiltInMethodValue instantiate(Object thisValue) {
        return this.isAttribute
                ? new BuiltInAttributeValue(
                        this.name, this.documentation, this.methodBody, thisValue)
                : new BuiltInMethodValue(
                        this.name,
                        this.documentation,
                        this.paramNames,
                        this.paramDefaults,
                        this.methodBody,
                        thisValue);
    }

    // ----- Class methods -----

    /**
     * Create a new methods factory wrapping an existing function value. The first function
     * parameter is considered as the "this" parameter.
     */
    public static BuiltInMethodFactory fromFunctionValue(
            BuiltInFunctionValue value, boolean isAttribute) {
        String[] paramNames = value.parameterNames.clone();
        Expr[] paramDefaults = value.parameterDefaultValues.clone();
        paramNames[0] = "this";
        paramDefaults[0] = null;
        return new BuiltInMethodFactory(
                value.name,
                value.documentation,
                paramNames,
                paramDefaults,
                (BuiltInBody) value.rootNode.getBody(),
                isAttribute);
    }

    /** Util function to create a map entry associating the method name with its value. */
    public static Map.Entry<String, BuiltInMethodFactory> createMethod(
            String name,
            String doc,
            String[] paramNames,
            Expr[] paramDefaults,
            BuiltInBody.BuiltInCallback callback) {
        return createMethod(
                name, doc, paramNames, paramDefaults, BuiltInBody.fromCallback(callback));
    }

    /**
     * Util function to create a map entry associating the method name with its value.
     *
     * @param paramNames Names of the method parameters. This array SHOULDN'T contain the "this"
     *     parameter since it is automatically added by this method.
     * @param paramDefaults Default values for the method parameters. A null value will
     *     automatically be added at the start of it to represents the "this" default value.
     */
    public static Map.Entry<String, BuiltInMethodFactory> createMethod(
            String name, String doc, String[] paramNames, Expr[] paramDefaults, BuiltInBody body) {
        return Map.entry(
                name,
                new BuiltInMethodFactory(
                        name,
                        doc,
                        ArrayUtils.concat(new String[] {"this"}, paramNames),
                        ArrayUtils.concat(new Expr[] {null}, paramDefaults),
                        body,
                        false));
    }

    /** Create a map entry containing a method factory which will produce attribute values. */
    public static Map.Entry<String, BuiltInMethodFactory> createAttribute(
            String name, String doc, BuiltInBody body) {
        return Map.entry(
                name,
                new BuiltInMethodFactory(
                        name, doc, new String[] {"this"}, new Expr[] {null}, body, true));
    }
}
