//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Specialization;

/** This class contains all methods for the rewriting node type. */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.REWRITING_NODE })
public final class RewritingNodeMethods {

    @BuiltInMethod(
        name = "clone",
        doc = "Given a rewriting node, clone it and return its copy",
        isProperty = true
    )
    public abstract static class CloneExpr extends BuiltInBody {

        @Specialization
        public Object execute(Libadalang.RewritingNode node) {
            return node.clone();
        }
    }
}
