//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.tools.TextWriter;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLCallable;
import com.adacore.lkql_jit.values.interop.LKQLCallable.CallableKind;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.stream.Stream;
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
        // Create an execution context with the default configuration
        Context context = Context.newBuilder(Constants.LKQL_ID)
            .allowAllAccess(true)
            .option("lkql.options", new LKQLOptions.Builder().build().toJson().toString())
            .build();

        // Create directory tree for the result
        try {
            Files.createDirectories(FileSystems.getDefault().getPath(outputDir));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        // If required, generate the documentation for the LKQL built-ins and prelude
        if (documentStd) {
            var path = FileSystems.getDefault().getPath(outputDir, "std.rst");
            try (BufferedWriter writer = Files.newBufferedWriter(path)) {
                // TODO: This eval prints the result on stdout, because we have to use
                //  "interactive" in order for the eval of the TopLevelList to return its result.
                // We need to think about how to fix that at the language level, so that we don't
                // have to call `eval` with `interactive=true` below.
                var res = context.eval(
                    Source.newBuilder(Constants.LKQL_ID, "document_builtins()", "<input>")
                        .interactive(true)
                        .build()
                );
                writer.write(res.toString());
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        // Then generate documentation for required LKQL modules
        if (modules != null) {
            for (var module : modules) {
                var outputFile = FileSystems.getDefault().getPath(outputDir, module + ".rst");
                try (BufferedWriter writer = Files.newBufferedWriter(outputFile)) {
                    System.out.print(
                        "Generating documentation for LKQL module \"" + module + "\" ..."
                    );
                    writer.write(documentModule(context, module));
                    System.out.println(" Done");
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        }

        return 0;
    }

    /** Get the RST documentation of an LKQL module from its name. */
    private static String documentModule(Context context, String module) {
        try (
            TextWriter moduleDoc = new TextWriter(new StringWriter());
            TextWriter functionsDoc = new TextWriter(new StringWriter());
            TextWriter selectorsDoc = new TextWriter(new StringWriter());
        ) {
            // First get the namespace object representing the module to document
            LKQLBaseNamespace namespace = context
                .eval(
                    Source.newBuilder(Constants.LKQL_ID, "import " + module, "<module_resolution>")
                        .uri(Paths.get("").toUri())
                        .build()
                )
                .as(LKQLBaseNamespace.class);
            LKQLBaseNamespace moduleNamespace = (LKQLBaseNamespace) namespace.getUncached(module);

            // Add the module's name as a header
            var header = module + "'s API doc";
            moduleDoc.write(header + "\n");
            moduleDoc.write("-".repeat(header.length()));
            moduleDoc.write("\n\n");

            // Prepare a map for callable documentations
            Map<CallableKind, TextWriter> callableDocs = Map.of(
                CallableKind.FUNCTION,
                functionsDoc,
                CallableKind.SELECTOR,
                selectorsDoc
            );

            // Then generate documentation for all module's callable values
            moduleNamespace
                .asMap()
                .values()
                .stream()
                .flatMap(value -> {
                    if (value instanceof LKQLCallable callable) {
                        return Stream.of(callable);
                    } else {
                        return Stream.empty();
                    }
                })
                .sorted(Comparator.comparing(callable -> callable.name))
                .forEach(callable -> {
                    var doc = callableDocs.get(callable.kind);
                    if (doc != null) {
                        documentCallable(doc, callable);
                    }
                });

            // Display the 'Functions' section
            moduleDoc.write("Functions\n");
            moduleDoc.write("^^^^^^^^^\n");
            moduleDoc.write(functionsDoc.toString());

            // Display the 'Selectors' section
            moduleDoc.write("Selectors\n");
            moduleDoc.write("^^^^^^^^^\n");
            moduleDoc.write(selectorsDoc.toString());

            // Finally return the module doc
            return moduleDoc.toString();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Generate the RST documentation for an LKQL callable values and add it to the provided output
     * buffer.
     */
    private static void documentCallable(TextWriter output, LKQLCallable callable) {
        output.write(".. function:: " + callable.profile() + "\n\n");
        output.withIndent(() -> {
            output.write(callable.documentation);
        });
        output.write("\n\n");
    }
}
