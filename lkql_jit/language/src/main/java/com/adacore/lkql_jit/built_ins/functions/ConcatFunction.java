//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

/**
 * This class represents the "concat" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ConcatFunction {

    public static final String NAME = "concat";

    /** Get a brand new "concat" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a list of lists or strings, return a concatenated list or string",
                new String[] {"lists"},
                new Expr[] {null},
                new SpecializedBuiltInBody<>(ConcatFunctionFactory.ConcatExprNodeGen.create()) {
                    @Override
                    protected Object dispatch(Object[] args) {
                        return specializedNode.executeConcat(args[0]);
                    }
                });
    }

    /** Expression of the "concat" function. */
    abstract static class ConcatExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract Object executeConcat(Object list);

        protected static boolean isString(Object o) {
            return LKQLTypeSystemGen.isString(o);
        }

        protected static boolean isList(Object o) {
            return LKQLTypeSystemGen.isLKQLList(o);
        }

        @Specialization(guards = {"list.size() > 0", "isString(list.get(0))"})
        protected String onListOfStrings(LKQLList list) {
            // Create a string builder and add all strings in the list
            String result = LKQLTypeSystemGen.asString(list.get(0));
            for (int i = 1; i < list.size(); i++) {
                final Object item = list.get(i);
                if (!LKQLTypeSystemGen.isString(item)) {
                    this.invalidElemType(list, item);
                }
                result = StringUtils.concat(result, LKQLTypeSystemGen.asString(item));
            }
            return result;
        }

        @Specialization(guards = {"list.size() > 0", "isList(list.get(0))"})
        protected LKQLList onListOfLists(LKQLList list) {
            Object[] result = LKQLTypeSystemGen.asLKQLList(list.get(0)).getContent();
            for (int i = 1; i < list.size(); i++) {
                final Object item = list.get(i);
                if (!LKQLTypeSystemGen.isLKQLList(item)) {
                    this.invalidElemType(list, item);
                }
                result = ArrayUtils.concat(result, LKQLTypeSystemGen.asLKQLList(item).getContent());
            }
            return new LKQLList(result);
        }

        @Specialization(guards = "notValidElem.size() > 0")
        @CompilerDirectives.TruffleBoundary
        protected LKQLList invalidElemType(
                @SuppressWarnings("unused") LKQLList notValidElem,
                @Cached("notValidElem.get(0)") Object elem) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST
                            + " of "
                            + LKQLTypesHelper.typeUnion(
                                    LKQLTypesHelper.LKQL_LIST, LKQLTypesHelper.LKQL_STRING),
                    LKQLTypesHelper.fromJava(elem) + " element",
                    body.argNode(0));
        }

        @Specialization(guards = "emptyList.size() == 0")
        protected LKQLList onEmptyList(@SuppressWarnings("unused") LKQLList emptyList) {
            return new LKQLList(new Object[0]);
        }

        @Fallback
        protected LKQLList invalidType(Object notValid) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST, LKQLTypesHelper.fromJava(notValid), body.argNode(0));
        }
    }
}
