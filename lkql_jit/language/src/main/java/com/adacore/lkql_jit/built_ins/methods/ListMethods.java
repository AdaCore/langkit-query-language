//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
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
        protected LKQLList onValid(LKQLList list, long low, long high) {
            // Offset the low bound by 1 since LKQL is 1-indexed
            low = low - 1;

            // Check bounds validity
            if (low < 0) {
                throw LKQLRuntimeException.invalidIndex((int) low + 1, this);
            } else if (high > list.getContent().length) {
                throw LKQLRuntimeException.invalidIndex((int) high, this);
            }

            // Return the sublist
            return new LKQLList(Arrays.copyOfRange(list.getContent(), (int) low, (int) high));
        }
    }
}
