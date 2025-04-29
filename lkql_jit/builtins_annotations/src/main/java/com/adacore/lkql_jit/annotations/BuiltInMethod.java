//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.annotations;

import java.lang.annotation.*;

/**
 * Annotation to add a new built-in method to LKQL. Those annotations are automatically processed by
 * the BuiltInProcessor Java processor.
 */
@Target(ElementType.TYPE)
public @interface BuiltInMethod {
    /** Name for this built-in method. */
    String name() default "";

    /** Documentation for this built-in method. */
    String doc() default "";

    /**
     * Types that this method should be added onto. Note that if the final computed set is empty,
     * method will be added on all types.
     */
    String[] targetTypes() default {};

    /**
     * Whether method is a property. A property is a built-in method that doesn't need any
     * arguments, and that doesn't take parens when you call it.
     */
    boolean isProperty() default false;
}
