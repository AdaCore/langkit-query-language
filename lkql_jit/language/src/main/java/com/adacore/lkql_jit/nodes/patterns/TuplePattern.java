//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.built_ins.values.LKQLTuple;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

public abstract class TuplePattern extends ValuePattern {
    /** The sub-patterns for this tuple pattern. */
    @Node.Children private final BasePattern[] patterns;

    public TuplePattern(SourceSection location, BasePattern[] patterns) {
        super(location);
        this.patterns = patterns;
    }

    @Specialization
    @ExplodeLoop
    public boolean onTuple(VirtualFrame frame, LKQLTuple tuple) {
        if (tuple.getArraySize() != patterns.length) {
            return false;
        }

        for (int i = 0; i < tuple.getArraySize(); i++) {
            if (!patterns[i].executeValue(frame, tuple.get(i))) {
                return false;
            }
        }
        return true;
    }

    @Fallback
    public boolean onOther(
            @SuppressWarnings("unused") VirtualFrame frame,
            @SuppressWarnings("unused") Object other) {
        return false;
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
