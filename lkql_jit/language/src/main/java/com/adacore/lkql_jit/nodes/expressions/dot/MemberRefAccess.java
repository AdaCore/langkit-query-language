//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
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
    private final Libadalang.MemberReference memberRef;

    // ----- Constructors -----

    public MemberRefAccess(SourceSection location, Identifier receiver, Identifier member) {
        super(location);

        // Get the Ada node description and check that the member exists
        final var description = Libadalang.NODE_DESCRIPTION_MAP.get(receiver.getName());
        if (description == null) {
            throw LKQLRuntimeException.invalidKindName(receiver);
        } else if (!description.fieldDescriptions.containsKey(member.getName())) {
            throw LKQLRuntimeException.noSuchField(member);
        }
        this.memberRef = description.fieldDescriptions.get(member.getName()).memberRef;
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.memberRef;
    }

    @Override
    public Libadalang.MemberReference executeMemberReference(VirtualFrame frame) {
        return this.memberRef;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(indentLevel);
    }
}
