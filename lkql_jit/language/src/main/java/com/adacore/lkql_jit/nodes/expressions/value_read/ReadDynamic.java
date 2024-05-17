//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * A dynamic read from the global interactive scope. Such reads are not optimized by the compiler.
 * We might want to investigate optimizing them at some stage.
 */
public class ReadDynamic extends Expr {

    private final String name;

    public ReadDynamic(final SourceSection location, String name) {
        super(location);
        this.name = name;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.getObject();
    }

    @CompilerDirectives.TruffleBoundary
    public Object getObject() {
        return LKQLLanguage.getContext(this).getGlobal().getGlobalObjects().get(this.name);
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"name"}, new Object[] {this.name});
    }
}
