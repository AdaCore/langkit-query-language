//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createMethod;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.built_ins.BuiltInsHolder;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.built_ins.functions.UniqueFunction;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import java.util.Arrays;
import java.util.Map;

/**
 * This class contains all built-in methods for the list type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public class ListMethods {

    public static final Map<String, BuiltInMethodFactory> methods =
            BuiltInsHolder.combine(
                    Map.ofEntries(
                            Map.entry(
                                    UniqueFunction.NAME,
                                    BuiltInMethodFactory.fromFunctionValue(
                                            UniqueFunction.getValue(), true)),
                            createMethod(
                                    "sublist",
                                    "Return a sublist of `list` from "
                                            + "`low_bound` to `high_bound`",
                                    new String[] {"low_bound", "high_bound"},
                                    new Expr[] {null, null},
                                    new SpecializedBuiltInBody<>(
                                            ListMethodsFactory.SublistExprNodeGen.create()) {
                                        @Override
                                        protected Object dispatch(Object[] args) {
                                            return this.specializedNode.executeSublist(
                                                    LKQLTypeSystemGen.asLKQLList(args[0]),
                                                    args[1],
                                                    args[2]);
                                        }
                                    })),
                    IterableMethods.methods);

    // ----- Inner classes -----

    /** Expression of the "sublist" method. */
    public abstract static class SublistExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract LKQLList executeSublist(LKQLList list, Object low, Object high);

        @Specialization
        protected LKQLList onValid(LKQLList list, long low, long high) {
            // Offset the low bound by 1 since LKQL is 1-indexed
            low = low - 1;

            // Check bounds validity
            if (low < 0) {
                throw LKQLRuntimeException.invalidIndex((int) low + 1, body.argNode(0));
            } else if (high > list.getContent().length) {
                throw LKQLRuntimeException.invalidIndex((int) high, body.argNode(1));
            }

            // Return the sublist
            return new LKQLList(Arrays.copyOfRange(list.getContent(), (int) low, (int) high));
        }

        @Specialization
        protected LKQLList onInvalidHigh(
                @SuppressWarnings("unused") LKQLList list,
                @SuppressWarnings("unused") long low,
                Object high) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_INTEGER, LKQLTypesHelper.fromJava(high), body.argNode(1));
        }

        @Fallback
        protected LKQLList onInvalidLow(
                @SuppressWarnings("unused") LKQLList list,
                Object low,
                @SuppressWarnings("unused") Object high) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_INTEGER, LKQLTypesHelper.fromJava(low), body.argNode(0));
        }
    }
}
