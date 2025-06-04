//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;

/**
 * This class is a POJO, it needs to be an LKQLNode
 * to simplify lowering and the resolution pass
 * (see {@link ResolutionPass})
 */
public class ClassDecl extends LKQLNode {

    public final String name;
    public final ArrayList<String> fields;

    public ClassDecl(SourceSection location, String name, ArrayList<String> fields) {
        super(location);
        this.name = name;
        this.fields = fields;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'executeGeneric'");
    }

    @Override
    public String toString(int indentLevel) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toString'");
    }
}
