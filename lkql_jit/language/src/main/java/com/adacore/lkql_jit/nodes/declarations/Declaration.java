//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents all declarations done in the LKQL: language imports, values or functions.
 *
 * @author Hugo GUERRIER
 */
public abstract class Declaration extends LKQLNode {

    // ----- Children -----

    /** Annotation of the declaration. */
    @Child
    protected Annotation annotation;

    // ----- Constructors -----

    /**
     * Create the declaration node.
     *
     * @param location The location of the node in the source.
     * @param annotation The annotation associated with the declaration.
     */
    protected Declaration(SourceSection location, Annotation annotation) {
        super(location);
        this.annotation = annotation;
    }
}
