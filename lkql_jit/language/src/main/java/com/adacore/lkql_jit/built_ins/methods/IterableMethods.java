//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBoolean;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBooleanNodeGen;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLTuple;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterator;
import com.adacore.lkql_jit.runtime.values.lists.BaseLKQLLazyList;
import com.adacore.lkql_jit.runtime.values.lists.LKQLFlattenResult;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.runtime.values.lists.LKQLMapResult;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;

/** This class contains all built-in methods for the iterable type in the LKQL language. */
@BuiltinMethodContainer(
    targetTypes = {
        LKQLTypesHelper.LKQL_LAZY_LIST,
        LKQLTypesHelper.LKQL_SELECTOR_LIST,
        LKQLTypesHelper.LKQL_LIST,
    }
)
public class IterableMethods {

    @BuiltInMethod(
        name = "enumerate",
        doc = """
        Return the content of the iterable object with each element associated \
        to its index in a tuple: [(<index>, <elem>), ...]""",
        isProperty = true
    )
    abstract static class EnumerateExpr extends BuiltInBody {

        @Specialization
        public LKQLList doGeneric(LKQLList receiver) {
            // Create the result array and fill it
            final var resContent = new Object[(int) receiver.size()];
            long index = 1;
            final var iterator = receiver.iterator();
            while (iterator.hasNext()) {
                resContent[(int) index - 1] = new LKQLTuple(
                    new Object[] { index, iterator.next() }
                );
                index++;
            }

            // Return the result
            return new LKQLList(resContent);
        }
    }

    @BuiltInMethod(name = "to_list", doc = "Transform into a list", isProperty = true)
    abstract static class ToListExpr extends BuiltInBody {

        @Specialization
        public LKQLList doGeneric(Iterable self) {
            // Create a new list from the iterable
            var res = new Object[(int) self.size()];
            Iterator iterator = self.iterator();
            for (int i = 0; i < res.length; i++) {
                res[i] = iterator.next();
            }

            // Return the new list value
            return new LKQLList(res);
        }
    }

    @BuiltInMethod(name = "length", doc = "Return the length of the iterable", isProperty = true)
    abstract static class LengthExpr extends BuiltInBody {

        @Specialization
        public long doGeneric(Iterable self) {
            return self.size();
        }
    }

    @BuiltInMethod(
        name = "map",
        doc = """
        Given an iterable and a function that takes one argument and return a value, return \
        a new iterable, result of the application of the function on all iterable elements.
        The returned iterable value is lazy."""
    )
    abstract static class MapExpr extends BuiltInBody {

        @Specialization(
            limit = Constants.SPECIALIZED_LIB_LIMIT,
            guards = "function.parameterNames.length == 1"
        )
        protected BaseLKQLLazyList onIterable(
            Iterable iterable,
            LKQLFunction function,
            @CachedLibrary("function") InteropLibrary functionLibrary
        ) {
            return new LKQLMapResult(iterable, function, functionLibrary);
        }
    }

    @BuiltInMethod(
        name = "flatten",
        doc = """
        Given an iterable of iterables, flatten all of them in a resulting iterable value. The \
        returned value is lazy.""",
        isProperty = true
    )
    abstract static class FlattenExpr extends BuiltInBody {

        @Specialization
        protected BaseLKQLLazyList onIterable(Iterable iterable) {
            return new LKQLFlattenResult(iterable);
        }
    }

    @BuiltInMethod(
        name = "flat_map",
        doc = """
        Given an iterable and a function that takes one argument and return another iterable \
        value, return a new iterable, result of the function application on all elements, flatten \
        in a sole iterable value. The returned iterable value is lazy."""
    )
    abstract static class FlatMapExpr extends BuiltInBody {

        @Specialization(
            limit = Constants.SPECIALIZED_LIB_LIMIT,
            guards = "function.parameterNames.length == 1"
        )
        protected BaseLKQLLazyList onIterable(
            Iterable iterable,
            LKQLFunction function,
            @CachedLibrary("function") InteropLibrary functionLibrary
        ) {
            return new LKQLFlattenResult(new LKQLMapResult(iterable, function, functionLibrary));
        }
    }

    @BuiltInMethod(
        name = "any",
        doc = "Given a collection and a predicate, returns true if any element satisfies the predicate."
    )
    abstract static class AnyExpr extends BuiltInBody {

        @Child
        private static LKQLToBoolean toBoolean = LKQLToBooleanNodeGen.create();

        @Specialization(
            limit = Constants.SPECIALIZED_LIB_LIMIT,
            guards = "predicate.parameterNames.length == 1"
        )
        protected Object onValidArgs(
            Iterable iterable,
            LKQLFunction predicate,
            @CachedLibrary("predicate") InteropLibrary functionLibrary
        ) {
            Iterator iterator = iterable.iterator();
            while (iterator.hasNext()) {
                try {
                    if (
                        toBoolean.execute(
                            functionLibrary.execute(
                                predicate,
                                predicate.closure.getContent(),
                                iterator.next()
                            )
                        )
                    ) return true;
                } catch (
                    ArityException | UnsupportedTypeException | UnsupportedMessageException e
                ) {
                    throw LKQLRuntimeException.fromJavaException(e, this);
                }
            }
            return false;
        }
    }

    @BuiltInMethod(
        name = "all",
        doc = "Given a collection and a predicate, returns true if all elements satisfies the predicate."
    )
    abstract static class AllExpr extends BuiltInBody {

        @Child
        private static LKQLToBoolean toBoolean = LKQLToBooleanNodeGen.create();

        @Specialization(
            limit = Constants.SPECIALIZED_LIB_LIMIT,
            guards = "predicate.parameterNames.length == 1"
        )
        protected Object onValidArgs(
            Iterable iterable,
            LKQLFunction predicate,
            @CachedLibrary("predicate") InteropLibrary functionLibrary
        ) {
            Iterator iterator = iterable.iterator();
            while (iterator.hasNext()) {
                try {
                    if (
                        !toBoolean.execute(
                            functionLibrary.execute(
                                predicate,
                                predicate.closure.getContent(),
                                iterator.next()
                            )
                        )
                    ) return false;
                } catch (
                    ArityException | UnsupportedTypeException | UnsupportedMessageException e
                ) {
                    throw LKQLRuntimeException.fromJavaException(e, this);
                }
            }
            return true;
        }
    }
}
