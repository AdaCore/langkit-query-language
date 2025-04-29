//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.utils;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.LKQLObject;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObjectLibrary;

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

    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected LKQLObject doObjects(
        LKQLObject left,
        LKQLObject right,
        LKQLNode caller,
        @CachedLibrary("left") DynamicObjectLibrary leftLib,
        @CachedLibrary("right") DynamicObjectLibrary rightLib,
        @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) DynamicObjectLibrary resLib,
        @Cached ConcatenationNode innerConcat
    ) {
        // Create the result object
        LKQLObject res = new LKQLObject(LKQLObject.emptyShape());

        // Insert all keys of the left object in the result, resolving conflicts by merging
        // values.
        for (var key : leftLib.getKeyArray(left)) {
            if (!rightLib.containsKey(right, key)) {
                resLib.put(res, key, leftLib.getOrDefault(left, key, null));
            } else {
                resLib.put(
                    res,
                    key,
                    innerConcat.execute(
                        leftLib.getOrDefault(left, key, null),
                        rightLib.getOrDefault(right, key, null),
                        caller
                    )
                );
            }
        }

        // Insert keys from the right object that aren't in the resulting object
        for (var key : rightLib.getKeyArray(right)) {
            if (!resLib.containsKey(res, key)) {
                resLib.put(res, key, rightLib.getOrDefault(right, key, null));
            }
        }

        // Return the resulting object
        return res;
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
