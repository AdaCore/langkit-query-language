//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.util.List;

/**
 * This class is a POJO, it needs to be an LKQLNode
 * to simplify lowering and the resolution pass
 * (see {@link ResolutionPass})
 */
public class AddBlock extends LKQLNode {

    public final List<ClassDecl> classes;
    public final List<PrefixField> fields;

    public AddBlock(SourceSection location, List<ClassDecl> classes, List<PrefixField> fields) {
        super(location);
        this.classes = classes;
        this.fields = fields;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(
            indentLevel,
            new String[] { "classes", "fields" },
            new Object[] { classes, fields }
        );
    }
}
