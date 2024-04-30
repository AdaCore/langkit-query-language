//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.built_ins.values.LKQLUnit;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This class represents a value declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ValueDeclaration extends Declaration {

    // ----- Attributes -----

    /** Frame slot to place the value in. */
    private final int slot;

    // ----- Children -----

    /** The expression representing the value of the variable. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr value;

    // ----- Constructors -----

    /**
     * Create a new value declaration node.
     *
     * @param location The location of the node in the source.
     * @param slot The frame slot to place the value in.
     * @param value The expression representing the value.
     */
    public ValueDeclaration(final SourceSection location, final int slot, final Expr value) {
        super(location, null);
        this.slot = slot;
        this.value = value;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        FrameUtils.writeLocal(frame, this.slot, this.value.executeGeneric(frame));
        return LKQLUnit.INSTANCE;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"slot"}, new Object[] {this.slot});
    }
}
