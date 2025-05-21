//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createMethod;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.nodes.utils.ValueCombiner;
import com.adacore.lkql_jit.nodes.utils.ValueCombinerNodeGen;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.util.Map;
import java.util.Map.Entry;

public final class ObjectMethods {

    public static final Entry<String, BuiltInMethodFactory> combineEntry =
            createMethod(
                    "combine",
                    "Combine two LKQL values if possible and return the result, recursively"
                            + " if required",
                    new String[] {"right", "recursive"},
                    new Expr[] {null, new BooleanLiteral(null, true)},
                    new CombineExpr());

    public static final Map<String, BuiltInMethodFactory> methods = Map.ofEntries(combineEntry);

    // ----- Inner classes -----

    /** Expression for the "combine" method. */
    public static final class CombineExpr extends AbstractBuiltInFunctionBody {

        private static final ValueCombiner combiner = ValueCombinerNodeGen.create();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get object to combine
            Object left = frame.getArguments()[0];
            Object right = frame.getArguments()[1];

            // Get whether the combination should be recursive
            boolean recursive;
            try {
                recursive = LKQLTypeSystemGen.expectBoolean(frame.getArguments()[2]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_BOOLEAN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        callNode.getArgList().getArgs()[2]);
            }

            // Then call the combination node
            return combiner.execute(left, right, recursive, this.callNode);
        }
    }
}
