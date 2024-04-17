//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.selectors;

import com.adacore.lkql_jit.built_ins.values.LKQLSelector;

/**
 * This interface defines the LKQL built-in selector factories.
 *
 * @author Hugo GUERRIER
 */
public interface BuiltInSelector {

    /**
     * Get the name of the build in selector.
     *
     * @return The name of the selector.
     */
    String getName();

    /**
     * Get the value of the built-in selector.
     *
     * @return The selector value.
     */
    LKQLSelector getValue();
}
