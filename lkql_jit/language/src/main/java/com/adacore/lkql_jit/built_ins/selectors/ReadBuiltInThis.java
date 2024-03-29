//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.selectors;

import com.adacore.lkql_jit.built_ins.values.LKQLDepthValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This expression represents the "this" variable reading for the built-in selectors.
 *
 * @author Hugo GUERRIER
 */
public final class ReadBuiltInThis extends Expr {

    /** Create a new read "this" node. */
    public ReadBuiltInThis() {
        super(null);
    }

    /**
     * Get the "this" variable that is the first argument of the frame.
     *
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return ((LKQLDepthValue) frame.getArguments()[1]).value;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return "BUILT_IN NODE";
    }
}
