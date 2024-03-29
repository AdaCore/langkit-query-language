//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.utils.source_location.SourceLocation;

/**
 * This node represents all unfiltered patterns in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class UnfilteredPattern extends BasePattern {

    /**
     * Create a new unfiltered pattern.
     *
     * @param location The location of the node in the source.
     */
    protected UnfilteredPattern(SourceLocation location) {
        super(location);
    }
}
