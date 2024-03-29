//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;

/**
 * This node represents an unnamed argument in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ExprArg extends Arg {

    // ----- Constructor -----

    /**
     * Create a new expression argument node.
     *
     * @param location The location of the node in the sources.
     * @param expr The expression of the argument.
     */
    public ExprArg(SourceLocation location, Expr expr) {
        super(location, null, expr);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
