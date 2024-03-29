//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.FileUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class represents the "base_name" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class BaseNameFunction {

    // ----- Attributes -----

    /** The name of the built-in. */
    public static final String NAME = "base_name";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a string that represents a file name, returns the basename",
                new String[] {"str"},
                new Expr[] {null},
                (VirtualFrame frame, FunCall call) -> {
                    // Get the file full path
                    Object path = frame.getArguments()[0];

                    // Check the argument type
                    if (!LKQLTypeSystemGen.isString(path)) {
                        throw LKQLRuntimeException.wrongType(
                                LKQLTypesHelper.LKQL_STRING,
                                LKQLTypesHelper.fromJava(path),
                                call.getArgList().getArgs()[0]);
                    }

                    // Return the base name of the file
                    return FileUtils.baseName(LKQLTypeSystemGen.asString(path));
                });
    }
}
