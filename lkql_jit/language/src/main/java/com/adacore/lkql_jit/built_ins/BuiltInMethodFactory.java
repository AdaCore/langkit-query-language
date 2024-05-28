//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.nodes.expressions.Expr;
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

    // ----- Constructors -----

    /** Create a new method factory directly with the method body */
    public BuiltInMethodFactory(
            String name,
            String documentation,
            String[] paramNames,
            Expr[] paramDefaults,
            BuiltInBody methodBody) {
        this.name = name;
        this.documentation = documentation;
        this.paramNames = paramNames;
        this.paramDefaults = paramDefaults;
        this.methodBody = methodBody;
    }

    // ----- Instance methods -----

    /** Instantiate the method with the given "thisValue" and return the LKQL value. */
    public BuiltInMethodValue instantiate(Object thisValue) {
        return new BuiltInMethodValue(
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
    public static BuiltInMethodFactory fromFunctionValue(BuiltInFunctionValue value) {
        return new BuiltInMethodFactory(
                value.name,
                value.documentation,
                value.parameterNames,
                value.parameterDefaultValues,
                (BuiltInBody) value.rootNode.getBody());
    }

    /** Util function to create a map entry associating the method name with its value. */
    public static Map.Entry<String, BuiltInMethodFactory> create(
            String name,
            String doc,
            String[] paramNames,
            Expr[] paramDefaults,
            BuiltInBody.BuiltInCallback callback) {
        return Map.entry(
                name,
                new BuiltInMethodFactory(
                        name, doc, paramNames, paramDefaults, BuiltInBody.fromCallback(callback)));
    }

    /** Util function to create a map entry associating the method name with its value. */
    public static Map.Entry<String, BuiltInMethodFactory> create(
            String name, String doc, String[] paramNames, Expr[] paramDefaults, BuiltInBody body) {
        return Map.entry(
                name, new BuiltInMethodFactory(name, doc, paramNames, paramDefaults, body));
    }
}
