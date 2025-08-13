//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.langkit_translator.passes.ResolutionPass;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This is a POJO, it needs to be an LKQLNode
 * to simplify lowering and the resolution pass
 * (see {@link ResolutionPass})
 */
public class PrefixField extends LKQLNode {

    public final String prefix;
    public final String field;

    public PrefixField(SourceSection location, String prefix, String field) {
        super(location);
        this.prefix = prefix;
        this.field = field;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(
            indentLevel,
            new String[] { "prefix", "field" },
            new Object[] { prefix, field }
        );
    }
}
