//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.runtime.values.LKQLTuple;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Specialization;
import java.util.LinkedList;
import java.util.List;

/** This class contains all built-in methods for the iterable type in the LKQL language. */
@BuiltinMethodContainer(
        targetTypes = {
            LKQLTypesHelper.LKQL_LAZY_LIST,
            LKQLTypesHelper.LKQL_SELECTOR_LIST,
            LKQLTypesHelper.LKQL_LIST
        })
public class IterableMethods {

    @BuiltInMethod(
            name = "enumerate",
            doc =
                    """
                  Return the content of the iterable object with each element associated \
                  to its index in a tuple: [(<index>, <elem>), ...]""",
            isAttribute = true)
    abstract static class EnumerateExpr extends BuiltInBody {
        @Specialization
        public LKQLList execute(LKQLList receiver) {
            // Create the result array and fill it
            final var resContent = new Object[(int) receiver.size()];
            long index = 1;
            final var iterator = receiver.iterator();
            while (iterator.hasNext()) {
                resContent[(int) index - 1] = new LKQLTuple(new Object[] {index, iterator.next()});
                index++;
            }

            // Return the result
            return new LKQLList(resContent);
        }
    }

    @BuiltInMethod(name = "to_list", doc = "Transform into a list", isAttribute = true)
    abstract static class ToListExpr extends BuiltInBody {
        @Specialization
        public LKQLList onIterable(Iterable self) {
            // Create a new list from the iterable
            List<Object> resList = new LinkedList<>();
            Iterator iterator = self.iterator();
            while (iterator.hasNext()) {
                resList.add(iterator.next());
            }

            // Return the new list value
            return new LKQLList(resList.toArray(new Object[0]));
        }
    }

    @BuiltInMethod(name = "length", doc = "Return the length of the iterable", isAttribute = true)
    abstract static class LengthExpr extends BuiltInBody {
        @Specialization
        public long onIterable(Iterable self) {
            return self.size();
        }
    }
}
