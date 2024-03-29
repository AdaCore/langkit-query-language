//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.block_expression;

import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;

/**
 * This class represents a body part of a block expression.
 *
 * @author Hugo GUERRIER
 */
public abstract class BlockBody extends LKQLNode {

    // ----- Constructors -----

    /**
     * Create a new block body node.
     *
     * @param location The location of the node in the source.
     */
    protected BlockBody(SourceLocation location) {
        super(location);
    }
}
