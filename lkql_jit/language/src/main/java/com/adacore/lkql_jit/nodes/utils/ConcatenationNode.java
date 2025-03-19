//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.utils;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;

@GenerateInline(false)
public abstract class ConcatenationNode extends Node {

    // ---- Execution methods -----

    /** Concat two objects. */
    public abstract Object execute(Object left, Object right, LKQLNode caller);

    // ----- Specializations -----

    @Specialization
    protected String doStrings(String left, String right, LKQLNode caller) {
        return StringUtils.concat(left, right);
    }

    @Specialization
    protected LKQLList doLists(LKQLList left, LKQLList right, LKQLNode caller) {
        final int leftSize = (int) left.size();
        final int rightSize = (int) right.size();
        final Object[] resContent = new Object[leftSize + rightSize];
        System.arraycopy(left.content, 0, resContent, 0, leftSize);
        System.arraycopy(right.content, 0, resContent, leftSize, rightSize);
        return new LKQLList(resContent);
    }

    @Fallback
    protected void nonConcatenable(Object left, Object right, LKQLNode caller) {
        throw LKQLRuntimeException.unsupportedOperation(
            LKQLTypesHelper.fromJava(left),
            "&",
            LKQLTypesHelper.fromJava(right),
            caller
        );
    }
}
