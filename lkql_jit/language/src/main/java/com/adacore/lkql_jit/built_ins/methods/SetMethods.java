//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.lists.LKQLArrayList;
import com.adacore.lkql_jit.values.sets.LKQLOrderedHashSet;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;

/** This class contains all built-in methods for the set type in the LKQL language. */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.LKQL_SET })
public class SetMethods {

    @BuiltInMethod(
        name = "to_list",
        doc = "Returns a list with the elements from the set.",
        isProperty = true
    )
    abstract static class TolistExpr extends BuiltInBody {

        @CompilerDirectives.TruffleBoundary
        @Specialization
        public LKQLArrayList doGeneric(LKQLOrderedHashSet self) {
            return new LKQLArrayList(self.internalSet.toArray());
        }
    }

    @BuiltInMethod(name = "contains", doc = "Returns true if the element is in the set.")
    abstract static class ContainsExpr extends BuiltInBody {

        @CompilerDirectives.TruffleBoundary
        @Specialization
        public boolean doGeneric(LKQLOrderedHashSet self, Object element) {
            return self.internalSet.contains(element);
        }
    }

    @BuiltInMethod(name = "add", doc = "Add the element in the set.")
    abstract static class AddExpr extends BuiltInBody {

        @CompilerDirectives.TruffleBoundary
        @Specialization
        public LKQLOrderedHashSet doGeneric(LKQLOrderedHashSet self, Object element) {
            if (self.internalSet.contains(element)) {
                return self; // noop if element already in set
            } else {
                var set = new LKQLOrderedHashSet(self);
                set.internalSet.add(element);
                return set;
            }
        }
    }

    @BuiltInMethod(name = "union", doc = "Returns the union of both sets.")
    abstract static class UnionExpr extends BuiltInBody {

        @CompilerDirectives.TruffleBoundary
        @Specialization
        public LKQLOrderedHashSet doGeneric(LKQLOrderedHashSet self, LKQLOrderedHashSet other) {
            var union = new LKQLOrderedHashSet(self);
            union.internalSet.addAll(other.internalSet);
            return union;
        }
    }

    @BuiltInMethod(name = "inter", doc = "Returns the intersection of both sets.")
    abstract static class InterExpr extends BuiltInBody {

        @CompilerDirectives.TruffleBoundary
        @Specialization
        public LKQLOrderedHashSet doGeneric(LKQLOrderedHashSet self, LKQLOrderedHashSet other) {
            var inter = new LKQLOrderedHashSet(self);
            inter.internalSet.retainAll(other.internalSet);
            return inter;
        }
    }
}
