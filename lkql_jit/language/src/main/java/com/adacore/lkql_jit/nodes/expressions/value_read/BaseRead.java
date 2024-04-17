//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;

/**
 * This node is the base for all variable reading in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BaseRead extends Expr {

    // ----- Attributes -----

    /** The slot to read. */
    protected final int slot;

    // ----- Constructors -----

    /**
     * Create a new variable reading node.
     *
     * @param location The location of the node in the source.
     * @param slot The slot to read.
     */
    protected BaseRead(final SourceLocation location, final int slot) {
        super(location);
        this.slot = slot;
    }
}
