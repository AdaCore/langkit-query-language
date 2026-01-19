//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import com.oracle.truffle.api.object.Shape;

/** This class represents the base of LKQL namespaces. */
public abstract class LKQLBaseNamespace extends LKQLDynamicObject {

    // ----- Attributes -----

    /** User documentation for this namespace. */
    public final String documentation;

    // ----- Constructors -----

    public LKQLBaseNamespace(Shape shape, String documentation) {
        super(shape);
        this.documentation = documentation;
    }
}
