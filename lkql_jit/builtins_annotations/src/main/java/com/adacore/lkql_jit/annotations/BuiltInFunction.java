//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

/**
 * Annotation to add a new built-in function to LKQL. Those annotations are automatically processed
 * by the BuiltInProcessor Java processor.
 */
@Target(ElementType.TYPE)
public @interface BuiltInFunction {
    /** Name for this built-in function */
    String name();

    /** Documentation for this built-in function */
    String doc();
}
