//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

public abstract class ListPattern extends ValuePattern {

    /** The sub-patterns for this list pattern. */
    @Node.Children
    private final BasePattern[] patterns;

    public ListPattern(SourceSection location, BasePattern[] patterns) {
        super(location);
        this.patterns = patterns;
    }

    @Specialization
    public boolean onList(VirtualFrame frame, LKQLList list) {
        var lastMatch = 0;
        for (int i = 0; i < list.getArraySize(); i++) {
            var pattern = patterns[i];
            lastMatch = i;
            // SplatPattern matches the rest of the list
            if (pattern instanceof SplatPattern) {
                return pattern.executeValue(
                    frame,
                    new LKQLList(list.getSlice(i, list.getArraySize()))
                );
            }
            // Else, fail on the first pattern that fails
            else if (!pattern.executeValue(frame, list.get(i))) {
                return false;
            }
        }

        if (lastMatch < patterns.length - 1) {
            // If there are remaining patterns, there need to be only one, and it needs to be a
            // SplatPattern.
            if (
                lastMatch + 1 != patterns.length - 1 ||
                !(patterns[lastMatch + 1] instanceof SplatPattern)
            ) {
                return false;
            }
        }

        return true;
    }

    @Fallback
    public boolean onOther(
        @SuppressWarnings("unused") VirtualFrame frame,
        @SuppressWarnings("unused") Object other
    ) {
        return false;
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
