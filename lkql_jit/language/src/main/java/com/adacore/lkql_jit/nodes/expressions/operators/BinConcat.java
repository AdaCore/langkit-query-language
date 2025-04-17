//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.nodes.utils.ConcatenationNode;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the concatenation operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
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

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
