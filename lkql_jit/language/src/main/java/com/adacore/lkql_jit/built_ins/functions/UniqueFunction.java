//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.SpecializedBuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

/**
 * This class represents the "unique" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class UniqueFunction {

    public static final String NAME = "unique";

    /** Get a brand new "unique" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given collection, remove all identical elements in order to have only one instance"
                        + " of each",
                new String[] {"indexable"},
                new Expr[] {null},
                new SpecializedBuiltInBody<>(UniqueFunctionFactory.UniqueExprNodeGen.create()) {
                    @Override
                    protected Object dispatch(Object[] args) {
                        return specializedNode.executeUnique(args[0]);
                    }
                });
    }

    /** Expression of the "unique" function. */
    abstract static class UniqueExpr extends SpecializedBuiltInBody.SpecializedBuiltInNode {

        public abstract LKQLList executeUnique(Object indexable);

        @Specialization
        protected LKQLList onIndexable(Indexable indexable) {
            return new LKQLList(ArrayUtils.unique(indexable.getContent()).toArray(new Object[0]));
        }

        @Fallback
        protected LKQLList invalidType(Object notValid) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_ITERABLE,
                    LKQLTypesHelper.fromJava(notValid),
                    body.argNode(0));
        }
    }
}
