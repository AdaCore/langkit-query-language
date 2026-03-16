//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBoolean;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBooleanNodeGen;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.LKQLFunction;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.adacore.lkql_jit.values.lists.LKQLArrayList;
import com.adacore.lkql_jit.values.streams.BaseCachedStream;
import com.adacore.lkql_jit.values.streams.LKQLEnumerateResult;
import com.adacore.lkql_jit.values.streams.LKQLFlattenResult;
import com.adacore.lkql_jit.values.streams.LKQLMapResult;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import java.util.ArrayList;

/** This class contains all built-in methods for the iterable type in the LKQL language. */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.LKQL_STREAM, LKQLTypesHelper.LKQL_LIST })
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
        public Iterable doGeneric(Iterable self) {
            return new LKQLEnumerateResult(self);
        }
    }

    @BuiltInMethod(name = "to_list", doc = "Transform into a list", isProperty = true)
    abstract static class ToListExpr extends BuiltInBody {

        @Specialization
        public LKQLArrayList doList(LKQLArrayList self) {
            return self;
        }

        @Specialization
        public LKQLArrayList doGeneric(Iterable self) {
            // Create a new list from the iterable
            var tmp = new ArrayList<Object>();
            Iterator iterator = self.iterator();
            while (iterator.hasNext()) {
                tmp.add(iterator.next());
            }

            // Return the new list value
            return new LKQLArrayList(tmp.toArray());
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
        protected BaseCachedStream onIterable(
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
        protected BaseCachedStream onIterable(Iterable iterable) {
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
        protected BaseCachedStream onIterable(
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
                            functionLibrary.execute(predicate, predicate.closure, iterator.next())
                        )
                    ) return true;
                } catch (
                    ArityException
                    | UnsupportedTypeException
                    | UnsupportedMessageException e
                ) {
                    throw LKQLEngineException.create(e);
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
                            functionLibrary.execute(predicate, predicate.closure, iterator.next())
                        )
                    ) return false;
                } catch (
                    ArityException
                    | UnsupportedTypeException
                    | UnsupportedMessageException e
                ) {
                    throw LKQLEngineException.create(e);
                }
            }
            return true;
        }
    }
}
