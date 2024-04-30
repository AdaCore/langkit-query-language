//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.lkql_jit.nodes.patterns.ValuePattern;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the base of all pattern related to the node in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class NodePattern extends ValuePattern {

    /**
     * Create a new node pattern.
     *
     * @param location The location of the node in the source.
     */
    protected NodePattern(SourceSection location) {
        super(location);
    }
}
