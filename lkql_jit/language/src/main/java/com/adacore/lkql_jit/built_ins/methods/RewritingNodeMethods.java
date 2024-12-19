//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createAttribute;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.Map;

/** This class contains all methods for the rewriting node type. */
public final class RewritingNodeMethods {

    public static final Map<String, BuiltInMethodFactory> methods =
            Map.ofEntries(
                    createAttribute(
                            "clone",
                            "Given a rewriting node, clone it and return its copy",
                            new CloneExpr()));

    /** Body for the clone method */
    public static final class CloneExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asRewritingNode(frame.getArguments()[0]).clone();
        }
    }
}
