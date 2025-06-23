//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.oracle.truffle.api.CompilerDirectives;

/** This class represents the LKQL value of a built-in function. */
public class BuiltInFunctionValue extends LKQLFunction {

    public String[] stringDefaultVals;

    private boolean defaultValsEvald = false;

    // ----- Constructor -----

    /** Create a built-in function value. */
    public BuiltInFunctionValue(
        String name,
        String documentation,
        String[] names,
        String[] defaultValues,
        BuiltInBody body
    ) {
        super(
            new FunctionRootNode(null, null, false, body, name),
            Closure.EMPTY,
            name,
            documentation,
            names,
            new Expr[names.length]
        );
        stringDefaultVals = defaultValues;
    }

    // ----- Instance methods -----

    /**
     * Set the calling node to the function body to allow its access in the built-in expression.
     *
     * @param callNode The node which called the built-in.
     */
    public void setCallNode(FunCall callNode) {
        ((BuiltInBody) this.getBody()).setCallNode(callNode);
    }

    @Override
    public Expr[] getParameterDefaultValues() {
        if (stringDefaultVals != null && !defaultValsEvald) {
            for (int i = 0; i < parameterDefaultValues.length; i++) {
                if (stringDefaultVals[i] != null) {
                    parameterDefaultValues[i] = getDefaultValAt(i);
                }
            }
            defaultValsEvald = true;
        }
        return parameterDefaultValues;
    }

    @CompilerDirectives.TruffleBoundary
    private Expr getDefaultValAt(int i) {
        var prg =
            ((TopLevelList) LKQLLanguage.getLanguage(rootNode.getBody()).translate(
                    stringDefaultVals[i],
                    "<defaultval>"
                )).program;
        return (Expr) prg[0];
    }
}
