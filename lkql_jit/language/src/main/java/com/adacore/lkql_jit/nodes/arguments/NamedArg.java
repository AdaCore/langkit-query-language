//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;

/**
 * This node represents a named argument in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NamedArg extends Arg {

    // ----- Constructor -----

    /**
     * Create a new named argument node.
     *
     * @param location The location of the node in the sources.
     * @param name The name of the argument.
     * @param expr The expression of the argument.
     */
    public NamedArg(SourceLocation location, Identifier name, Expr expr) {
        super(location, name, expr);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(indentLevel, new String[] {"name"}, new Object[] {this.argName});
    }
}
