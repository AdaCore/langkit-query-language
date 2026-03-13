//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.match.MatchArm;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.util.List;

/**
 * This class is a POJO, it needs to be an LKQLNode
 * to simplify lowering and the resolution pass
 * (see {@link ResolutionPass})
 */
public class RewriteBlock extends LKQLNode {

    @Children
    private MatchArm[] arms;

    public RewriteBlock(SourceSection location, List<MatchArm> clauses) {
        super(location);
        arms = clauses.toArray(new MatchArm[0]);
    }

    public MatchArm[] getArms() {
        return arms;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLEngineException.shouldNotReachHere();
    }

    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(indentLevel, new String[] {}, new Object[] {});
    }
}
