//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

/**
 * Marks a class as a container for built-in method declarations. This allows declaring the target
 * types for all contained methods only once.
 */
@Target(ElementType.TYPE)
public @interface BuiltinMethodContainer {
    /** Target types for all built-in methods contained by this BuiltInMethodContainer. */
    String[] targetTypes();
}
