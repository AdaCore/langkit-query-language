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

package com.adacore.lkql_jit;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.util.List;
import java.util.concurrent.Callable;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import picocli.CommandLine;

@CommandLine.Command(name = "doc", description = "Generate API doc for LKQL modules, in RST format")
public class LKQLDoc implements Callable<Integer> {
    @CommandLine.Option(
            names = "--std",
            description = "Generate apidoc for the prelude & builtin functions")
    public boolean documentStd;

    @CommandLine.Option(
            names = {"-O", "--output-dir"},
            description = "Output directory for generated RST files")
    public String outputDir = ".";

    @CommandLine.Parameters(description = "LKQL modules to document")
    public List<String> modules;

    @Override
    public Integer call() {
        Context context = Context.newBuilder("lkql").allowAllAccess(true).build();
        try {
            Files.createDirectories(FileSystems.getDefault().getPath(outputDir));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        if (documentStd) {
            var path = FileSystems.getDefault().getPath(outputDir, "std.rst");
            try (BufferedWriter writer = Files.newBufferedWriter(path)) {
                // TODO: This eval prints the result on stdout, because we have to use
                //  "interactive" in order for the eval of the TopLevelList to return its result.
                // We need to think about how to fix that at the language level, so that we don't
                // have to call `eval` with `interactive=true` below.
                var res =
                        context.eval(
                                Source.newBuilder("lkql", "document_builtins()", "<input>")
                                        .interactive(true)
                                        .build());
                writer.write(res.toString());
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        for (var mod : modules) {
            var path = FileSystems.getDefault().getPath(outputDir, mod + ".rst");
            try (BufferedWriter writer = Files.newBufferedWriter(path)) {
                context.eval(
                        Source.newBuilder("lkql", "import " + mod, "<input>")
                                .interactive(true)
                                .build());
                var doc =
                        context.eval(
                                Source.newBuilder(
                                                "lkql",
                                                "document_namespace(" + mod + ",\"" + mod + "\")",
                                                "<input>")
                                        .interactive(true)
                                        .build());
                writer.write(doc.toString());
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        return 0;
    }
}
