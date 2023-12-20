/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.values.LKQLFunction;
import com.adacore.lkql_jit.built_ins.values.LKQLNamespace;
import com.adacore.lkql_jit.built_ins.values.LKQLSelector;
import com.adacore.lkql_jit.built_ins.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.TextWriter;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.io.StringWriter;
import java.util.Comparator;

public class DocumentNamespace {
    public static final String NAME = "document_namespace";

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Return a string in the RsT format containing documentation for all built-ins",
                new String[] {"namespace", "name"},
                new Expr[] {null, null},
                (VirtualFrame frame, FunCall call) -> impl(frame.materialize(), call));
    }

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
