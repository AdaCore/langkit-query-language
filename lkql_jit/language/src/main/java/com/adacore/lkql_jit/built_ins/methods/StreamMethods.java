//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.oracle.truffle.api.dsl.Specialization;

/** This class contains all built-in methods for the stream type in the LKQL language. */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.LKQL_STREAM })
public class StreamMethods {

    @BuiltInMethod(name = "head", doc = "Return the head of the stream.", isProperty = true)
    abstract static class HeadExpr extends BuiltInBody {

        @Specialization
        public Object doGeneric(LKQLStream self) {
            return self.getHead();
        }
    }

    @BuiltInMethod(name = "tail", doc = "Return the tail of the stream.", isProperty = true)
    abstract static class TailExpr extends BuiltInBody {

        @Specialization
        public LKQLStream doGeneric(LKQLStream self) {
            return self.getTail();
        }
    }

    // TODO: we want to remove this method when deprecating LKQL v1
    // (see https://gitlab.adacore-it.com/eng/libadalang/langkit-query-language/-/issues/616)
    @BuiltInMethod(
        name = "length",
        doc = "Return the length of the stream. WARNING: This may never return in case of an infinite stream.",
        isProperty = true
    )
    abstract static class LengthExpr extends BuiltInBody {

        @Specialization
        public long doGeneric(LKQLStream self) {
            var it = self.iterator();
            var length = 0;
            while (it.hasNext()) {
                it.next();
                length++;
            }
            return length;
        }
    }
}
