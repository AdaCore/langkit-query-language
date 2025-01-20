//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

/**
 * This class represents the LKQL value of an instantiated attribute. An attribute is a special
 * method with not other arguments than "this" and is called implicitly by the dotted-name notation.
 */
public class BuiltInPropertyValue extends BuiltInMethodValue {
    /** Create a new built-in attribute value. */
    public BuiltInPropertyValue(
            String name, String documentation, BuiltInBody body, Object thisValue) {
        super(name, documentation, new String[] {"this"}, new String[] {null}, body, thisValue);
    }
}
