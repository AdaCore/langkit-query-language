//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.utils.ConcatenationNode;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a concatenation operation. */
public abstract class BinConcat extends BinOp {

    // ----- Constructors -----

    /**
     * Create a concatenation node.
     *
     * @param location The location of the node in the source.
     */
    protected BinConcat(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    @Specialization
    protected Object doDispatch(Object left, Object right, @Cached ConcatenationNode concat) {
        return concat.execute(left, right, this);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
