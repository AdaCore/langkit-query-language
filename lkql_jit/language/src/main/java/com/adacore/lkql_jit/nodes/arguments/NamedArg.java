//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.arguments;

import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a named argument in a call expression. */
public final class NamedArg extends Arg {

    // ----- Constructor -----

    /**
     * Create a new named argument node.
     *
     * @param location The location of the node in the source.
     * @param name Name of the argument.
     * @param expr Expression of the argument.
     */
    public NamedArg(SourceSection location, Identifier name, Expr expr) {
        super(location, name, expr);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(
            indentLevel,
            new String[] { "name" },
            new Object[] { this.argName }
        );
    }
}
