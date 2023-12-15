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

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltInsHolder;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.utils.TextWriter;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.io.StringWriter;
import java.util.Map;

public class DocumentBuiltins {
    public static final String NAME = "document_builtins";

    @CompilerDirectives.TruffleBoundary
    public static String documentBuiltinsImpl(MaterializedFrame frame, FunCall call) {
        var sw = new StringWriter();
        try (TextWriter writer = new TextWriter(sw)) {
            writer.write("Standard library\n");
            writer.write("----------------\n");
            writer.write("\n");
            writer.write("Builtin functions\n");
            writer.write("^^^^^^^^^^^^^^^^^\n");
            writer.write("\n");

            for (var func : BuiltInsHolder.get().builtInFunctions) {
                writer.write(".. function:: ");
                writer.write(func.getName());
                writer.write("(" + String.join(", ", func.parameterNames) + ")");
                writer.write("\n\n");
                writer.withIndent(
                        () -> {
                            writer.write(func.documentation);
                        });
                writer.write("\n");
                writer.write("\n");
            }

            writer.write("Builtin selectors\n");
            writer.write("^^^^^^^^^^^^^^^^^\n");
            writer.write("\n");

            for (var sel : BuiltInsHolder.get().builtInSelectors) {
                writer.write(".. function:: ");
                writer.write(sel.getName());
                writer.write("()");
                writer.write("\n\n");
                writer.withIndent(
                        () -> {
                            writer.write(sel.getValue().lkqlDocumentation());
                        });
                writer.write("\n");
                writer.write("\n");
            }

            writer.write("Builtin methods\n");
            writer.write("^^^^^^^^^^^^^^^\n");
            writer.write("\n");

            var sortedBuiltinMethods =
                    new java.util.ArrayList<>(
                            BuiltInsHolder.get().builtInMethods.entrySet().stream()
                                    .sorted(Map.Entry.comparingByKey())
                                    .toList());

            sortedBuiltinMethods.add(0, Map.entry("Any", BuiltInsHolder.get().commonMethods));

            for (var entry : sortedBuiltinMethods) {

                var methods =
                        entry.getValue().entrySet().stream()
                                .sorted(Map.Entry.comparingByKey())
                                .toList();

                // Skip type if there are no methods to document
                if (methods.size() == 0) {
                    continue;
                }

                var typeName = entry.getKey();
                var header = "Methods for `" + typeName + "`";
                writer.write(header + "\n");
                writer.write("\"".repeat(header.length()) + "\n");

                for (var method : methods) {
                    writer.write(".. method:: ");
                    writer.write(typeName + "." + method.getKey());
                    writer.write("(" + String.join(", ", method.getValue().parameterNames) + ")");
                    writer.write("\n\n");
                    writer.withIndent(
                            () -> {
                                writer.write(method.getValue().documentation);
                            });
                    writer.write("\n");
                    writer.write("\n");
                }
            }

            return sw.getBuffer().toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Return a string in the RsT format containing documentation for all built-ins",
                new String[] {},
                new Expr[] {},
                (VirtualFrame frame, FunCall call) ->
                        documentBuiltinsImpl(frame.materialize(), call));
    }
}
