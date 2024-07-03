//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.built_ins.values.LKQLObject;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import com.oracle.truffle.api.source.SourceSection;

public abstract class ObjectPattern extends ValuePattern {

    @Node.Children private final BasePattern[] patterns;
    private final Shape shape;

    @Node.Child private SplatPattern splat;

    private final String[] keys;

    protected ObjectPattern(
            SourceSection location, BasePattern[] patterns, String[] keys, SplatPattern splat) {
        super(location);
        assert patterns.length == keys.length;
        this.patterns = patterns;
        this.keys = keys;
        this.splat = splat;
        this.shape = Shape.newBuilder().build();
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    public boolean onObject(
            VirtualFrame frame,
            final LKQLObject object,
            @CachedLibrary("object") DynamicObjectLibrary objectLibrary) {

        for (int i = 0; i < keys.length; i++) {
            var key = keys[i];
            var pattern = patterns[i];
            Object res = objectLibrary.getOrDefault(object, key, null);
            if (res == null || !(pattern.executeValue(frame, res))) {
                return false;
            }
        }

        var objKeys = objectLibrary.getKeyArray(object);
        if (objKeys.length > keys.length) {

            // If there is a splat pattern and the object is bigger than the pattern, then it's
            // OK: we'll return true, and bind the remaining keys if needed.

            if (splat != null) {
                // If the splat has a binding, we need to compute the new object without the keys
                // that have already been matched
                if (splat.hasBinding()) {
                    LKQLObject splatObject = new LKQLObject(shape);
                    DynamicObjectLibrary splatObjectLib = DynamicObjectLibrary.getUncached();

                    for (var objKey : objKeys) {
                        var keep = true;
                        // Check if this key is part of the keys we match as part of the pattern
                        // NOTE: There is probably a way to do better than this at a Truffle
                        // level, because the keys of the pattern are known statically. For the
                        // moment this is not a performance sensitive part of the code, so this
                        // is fine.
                        for (var patternKey : keys) {
                            if (ObjectUtils.equals(objKey, patternKey)) {
                                keep = false;
                                break;
                            }
                        }
                        // Add the assoc
                        if (keep) {
                            splatObjectLib.put(splatObject, objKey, object.getUncached(objKey));
                        }
                    }
                    // Splat always returns true, and binds the object
                    return splat.executeValue(frame, splatObject);
                } else {
                    return true;
                }
            }
            return false;
        }

        return true;
    }

    @Fallback
    public boolean onOther(
            @SuppressWarnings("unused") VirtualFrame frame,
            @SuppressWarnings("unused") Object other) {
        return false;
    }
}
