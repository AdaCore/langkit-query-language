//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.utils.functions.ArrayUtils;

/**
 * This class is a factory for a specific built-in method. This factory will build a new method
 * value each time the method is instantiated.
 */
public final class BuiltInMethodFactory {

    // ----- Attributes -----

    public final String name;

    public final String documentation;

    public final String[] paramNames;

    public String[] defaultValues;

    public final BuiltInBody methodBody;

    /**
     * Whether the factory should produce "attribute" value. See the {@link BuiltInPropertyValue}
     * class for more information.
     */
    public final boolean isProperty;

    // ----- Constructors -----

    /** Create a new method factory directly with the method body. */
    public BuiltInMethodFactory(
        String name,
        String documentation,
        String[] names,
        String[] defaultValues,
        BuiltInBody methodBody,
        boolean isProperty
    ) {
        this.name = name;
        this.documentation = documentation;
        this.paramNames = names;
        this.defaultValues = defaultValues;
        this.methodBody = methodBody;
        this.isProperty = isProperty;
    }

    // ----- Instance methods -----

    /** Instantiate the method with the given "thisValue" and return the LKQL value. */
    public BuiltInMethodValue instantiate(Object thisValue) {
        return this.isProperty
            ? new BuiltInPropertyValue(this.name, this.documentation, this.methodBody, thisValue)
            : new BuiltInMethodValue(
                this.name,
                this.documentation,
                this.paramNames,
                this.defaultValues,
                this.methodBody,
                thisValue
            );
    }

    // ----- Class methods -----

    /**
     * Util function to create a map entry associating the method name with its value.
     *
     * @param parameters Specs for parameters. This array SHOULDN'T contain the "this" parameter
     *     since it is automatically added by this method.
     */
    public static BuiltInMethodFactory createMethod(
        String name,
        String doc,
        String[] names,
        String[] defaultValues,
        BuiltInBody body
    ) {
        return new BuiltInMethodFactory(
            name,
            doc,
            ArrayUtils.concat(new String[] { "this" }, names),
            ArrayUtils.concat(new String[] { null }, defaultValues),
            body,
            false
        );
    }

    /** Create a map entry containing a method factory which will produce attribute values. */
    public static BuiltInMethodFactory createAttribute(String name, String doc, BuiltInBody body) {
        return new BuiltInMethodFactory(
            name,
            doc,
            new String[] { "this" },
            new String[] { null },
            body,
            true
        );
    }
}
