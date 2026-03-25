//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.lists.LKQLArrayList;
import com.oracle.truffle.api.dsl.Specialization;
import java.util.Arrays;

/** This class contains all built-in methods for the list type in the LKQL language. */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.LKQL_LIST })
public class ListMethods {

    @BuiltInMethod(
        name = "sublist",
        doc = "Return a sublist of `list` from `low_bound` to `high_bound`"
    )
    public abstract static class SublistExpr extends BuiltInBody {

        @Specialization
        protected LKQLArrayList onValid(LKQLArrayList list, long low, long high) {
            // Offset the low bound by 1 since LKQL is 1-indexed
            low = low - 1;

            // Check bounds validity
            if (low < 0) {
                throw LKQLRuntimeError.invalidIndex((int) low + 1, this);
            } else if (high > list.getContent().length) {
                throw LKQLRuntimeError.invalidIndex((int) high, this);
            }

            // Return the sublist
            return new LKQLArrayList(Arrays.copyOfRange(list.getContent(), (int) low, (int) high));
        }
    }

    @BuiltInMethod(name = "length", doc = "Return the length of the list", isProperty = true)
    abstract static class LengthExpr extends BuiltInBody {

        @Specialization
        public long doGeneric(LKQLArrayList self) {
            return self.size();
        }
    }
}
