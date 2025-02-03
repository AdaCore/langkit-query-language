//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import com.adacore.lkql_jit.options.LKQLOptions;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.util.List;
import java.util.concurrent.Callable;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import picocli.CommandLine;

@CommandLine.Command(
    name = "doc-api",
    description = "Generate API doc for LKQL modules, in RST format"
)
public class LKQLDocAPI implements Callable<Integer> {

    @CommandLine.Option(
        names = "--std",
        description = "Generate apidoc for the prelude & builtin functions"
    )
    public boolean documentStd;

    @CommandLine.Option(
        names = { "-O", "--output-dir" },
        description = "Output directory for generated RST files"
    )
    public String outputDir = ".";

    @CommandLine.Parameters(description = "LKQL modules to document")
    public List<String> modules;

    @Override
    public Integer call() {
        Context context = Context.newBuilder("lkql")
            .allowAllAccess(true)
            // Set default LKQL options
            .option("lkql.options", new LKQLOptions.Builder().build().toJson().toString())
            .build();
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
                var res = context.eval(
                    Source.newBuilder("lkql", "document_builtins()", "<input>")
                        .interactive(true)
                        .build()
                );
                writer.write(res.toString());
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        for (var mod : modules) {
            var path = FileSystems.getDefault().getPath(outputDir, mod + ".rst");
            try (BufferedWriter writer = Files.newBufferedWriter(path)) {
                context.eval(
                    Source.newBuilder("lkql", "import " + mod, "<input>").interactive(true).build()
                );
                var doc = context.eval(
                    Source.newBuilder(
                        "lkql",
                        "document_namespace(" + mod + ",\"" + mod + "\")",
                        "<input>"
                    )
                        .interactive(true)
                        .build()
                );
                writer.write(doc.toString());
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        return 0;
    }
}
