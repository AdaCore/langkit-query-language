//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a dot-access on a node kind. For now, such accesses only returns member
 * references, but it may evolve later in development.
 */
public class MemberRefAccess extends Expr {

    // ----- Attributes -----

    /** The member reference, result of this node execution. */
    private final LangkitSupport.MemberReferenceInterface memberRef;

    // ----- Constructors -----

    public MemberRefAccess(
        SourceSection location,
        LangkitSupport.MemberReferenceInterface memberRef
    ) {
        super(location);
        this.memberRef = memberRef;
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.memberRef;
    }

    @Override
    public LangkitSupport.MemberReferenceInterface executeMemberReference(VirtualFrame frame) {
        return this.memberRef;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(indentLevel);
    }
}
