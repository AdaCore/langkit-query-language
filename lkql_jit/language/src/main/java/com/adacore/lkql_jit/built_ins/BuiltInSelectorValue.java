//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.built_ins.values.LKQLSelector;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorArm;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.Closure;

/**
 * This class represents the base of the built-in selector values.
 *
 * @author Hugo GUERRIER
 */
public class BuiltInSelectorValue extends LKQLSelector {

    // ----- Constructors -----

    /**
     * Create a new built-in selector value.
     *
     * @param name The name of the selector.
     * @param documentation The documentation of the selector.
     * @param arms The arms for the selector execution.
     */
    public BuiltInSelectorValue(String name, String documentation, SelectorArm[] arms) {
        super(
                new SelectorRootNode(null, null, false, -1, -1, arms),
                Closure.EMPTY,
                name,
                documentation);
    }
}
