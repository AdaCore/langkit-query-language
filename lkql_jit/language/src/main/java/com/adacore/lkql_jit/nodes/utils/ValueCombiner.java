//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.utils;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.LKQLObject;
import com.adacore.lkql_jit.utils.Constants;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.source.SourceSection;

@GenerateInline(false)
public abstract class ValueCombiner extends Node {

    // ----- Execution methods -----

    /** Combine two values. */
    public abstract Object execute(
        Object left,
        Object right,
        boolean recursive,
        SourceSection newObjectLocation,
        LKQLNode caller
    );

    // ----- Specializations -----

    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected LKQLObject onObjects(
        LKQLObject left,
        LKQLObject right,
        boolean recursive,
        SourceSection newObjectLocation,
        LKQLNode caller,
        @CachedLibrary("left") DynamicObjectLibrary leftLib,
        @CachedLibrary("right") DynamicObjectLibrary rightLib,
        @CachedLibrary(limit = Constants.DISPATCHED_LIB_LIMIT) DynamicObjectLibrary resLib,
        @Cached ValueCombiner recursiveCombiner
    ) {
        // Create the result object
        LKQLObject res = new LKQLObject(LKQLObject.emptyShape(), newObjectLocation);

        // Insert all keys of the left object in the result, resolving conflicts by combining
        // values.
        for (var key : leftLib.getKeyArray(left)) {
            if (!rightLib.containsKey(right, key)) {
                resLib.put(res, key, leftLib.getOrDefault(left, key, null));
            } else if (recursive) {
                resLib.put(
                    res,
                    key,
                    recursiveCombiner.execute(
                        leftLib.getOrDefault(left, key, null),
                        rightLib.getOrDefault(right, key, null),
                        recursive,
                        newObjectLocation,
                        caller
                    )
                );
            } else {
                throw LKQLRuntimeException.objectCombiningCollision((String) key, caller);
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
    protected Object onOthers(
        Object left,
        Object right,
        boolean recursive,
        SourceSection newObjectLocation,
        LKQLNode caller,
        @Cached ConcatenationNode concatNode
    ) {
        return concatNode.execute(left, right, caller);
    }
}
