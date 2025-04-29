//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the pattern that filters value in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class ValuePattern extends UnfilteredPattern {

    /**
     * Create a new value pattern.
     *
     * @param location The token location in the source.
     */
    protected ValuePattern(SourceSection location) {
        super(location);
    }
}
