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
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.LKQLNamespace;
import com.adacore.lkql_jit.runtime.values.LKQLSelector;
import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.TextWriter;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.io.StringWriter;
import java.util.Comparator;

public class DocumentNamespace {
    public static final String NAME = "document_namespace";

    /** Get a brand new "document_namespace" function value. */
    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Return a string in the RsT format containing documentation for all built-ins",
                new String[] {"namespace", "name"},
                new Expr[] {null, null},
                (VirtualFrame frame, FunCall call) -> impl(frame.materialize(), call));
    }

    /** Function for the "document_namespace" execution. */
    @CompilerDirectives.TruffleBoundary
    private static Object impl(MaterializedFrame frame, FunCall call) {
        Object nsObj = frame.getArguments()[0];
        Object nameObj = frame.getArguments()[1];

        if (!LKQLTypeSystemGen.isLKQLNamespace(nsObj)) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_NAMESPACE,
                    LKQLTypesHelper.fromJava(nsObj),
                    call.getArgList().getArgs()[0]);
        }

        if (!LKQLTypeSystemGen.isString(nameObj)) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_STRING,
                    LKQLTypesHelper.fromJava(nameObj),
                    call.getArgList().getArgs()[1]);
        }

        LKQLNamespace namespace = LKQLTypeSystemGen.asLKQLNamespace(nsObj);
        String name = LKQLTypeSystemGen.asString(nameObj);

        var sw = new StringWriter();
        try (TextWriter writer = new TextWriter(sw)) {

            var header = name + "'s API doc";
            writer.write(header + "\n");
            writer.write("-".repeat(header.length()));
            writer.write("\n\n");

            writer.write("Functions\n");
            writer.write("^^^^^^^^^\n");

            var functions =
                    namespace.asMap().values().stream()
                            .filter(LKQLTypeSystemGen::isLKQLFunction)
                            .map(LKQLTypeSystemGen::asLKQLFunction)
                            .sorted(Comparator.comparing(LKQLFunction::getName));

            for (var func : functions.toList()) {
                documentCallable(writer, func);
            }

            writer.write("Selectors\n");
            writer.write("^^^^^^^^^\n");

            var selectors =
                    namespace.asMap().values().stream()
                            .filter(LKQLTypeSystemGen::isLKQLSelector)
                            .map(LKQLTypeSystemGen::asLKQLSelector)
                            .sorted(Comparator.comparing(LKQLSelector::lkqlProfile));

            for (var sel : selectors.toList()) {
                documentCallable(writer, sel);
            }

            return sw.getBuffer().toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static void documentCallable(TextWriter writer, BasicLKQLValue callable) {
        writer.write(".. function:: " + callable.lkqlProfile() + "\n\n");
        writer.withIndent(
                () -> {
                    writer.write(callable.lkqlDocumentation());
                });
        writer.write("\n\n");
    }
}
