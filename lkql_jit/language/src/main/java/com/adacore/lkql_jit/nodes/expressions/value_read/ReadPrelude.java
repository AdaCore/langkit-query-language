//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.LKQLLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ReadPrelude extends BaseRead {

    public ReadPrelude(final SourceSection location, final int slot) {
        super(location, slot);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return LKQLLanguage.getContext(this).getGlobal().preludeObjects[this.slot];
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"slot"}, new Object[] {this.slot});
    }
}
