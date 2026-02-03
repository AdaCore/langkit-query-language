//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.values.LKQLFunction;
import com.adacore.lkql_jit.values.interop.LKQLAnnotation;

/** This class represents the LKQL value of a built-in function. */
public class BuiltInFunctionValue extends LKQLFunction {

    // ----- Constructor -----

    /** Create a built-in function value. */
    public BuiltInFunctionValue(
        String name,
        String documentation,
        String[] names,
        Expr[] defaultValues,
        BuiltInBody body
    ) {
        this(
            documentation,
            new FunctionRootNode(
                LKQLLanguage.getLanguage(body),
                null,
                false,
                false,
                names,
                defaultValues,
                body,
                name
            )
        );
    }

    public BuiltInFunctionValue(String documentation, FunctionRootNode functionRootNode) {
        super(
            functionRootNode,
            Closure.EMPTY,
            documentation,
            functionRootNode.getParameterNames(),
            functionRootNode.getDefaultParameters(),
            functionRootNode.getBody(),
            new LKQLAnnotation[0]
        );
    }
}
