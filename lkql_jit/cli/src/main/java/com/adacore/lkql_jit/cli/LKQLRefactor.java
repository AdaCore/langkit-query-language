//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import static com.adacore.liblkqllang.Liblkqllang.*;

import com.adacore.lkql_jit.options.Refactorings.LKQLToLkt;
import com.adacore.lkql_jit.options.Refactorings.Refactoring;
import com.adacore.lkql_jit.options.Refactorings.TokenBasedRefactoring;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import picocli.CommandLine;

/**
 * Refactor command for LKQL. Allows to run automatic migrations, allowing the
 * LKQL team to create migrators to adapt to syntactic or semantic changes in
 * LKQL.
 *
 * <p>
 * Migrators are just an implementation of the functionnal interface
 * 'Refactoring'. It expects an LKQL unit as input and produces the new unit
 * content as a 'String'.
 *
 * <p>
 * In practice, a 'Refactoring' implementation will be of one of 2 kinds:
 * 1. A 'TokenBasedRefactoring' working on the stream of tokens directly and
 * producing rewriting actions on each token of the stream.
 * 2. A 'TreeBasedRefactoring' working on the Liblkqllang syntax tree and
 * producing a new 'String' directly.
 *
 * <p>
 * Then the rewriter will emit a new file for every LKQL unit, either inplace or
 * on stdout.
 *
 * <p>
 * To add a refactoring, you need to extend the 'RefactoringKind' enum to add a
 * new refactoring id, and then extend the 'getRefactoring' method to return the
 * corresponding implementation of 'Refactoring' to apply.
 *
 */
@CommandLine.Command(
    name = "refactor",
    description = "Automatically run a refactoring on LKQL code"
)
public class LKQLRefactor implements Callable<Integer> {

    @CommandLine.Spec
    public picocli.CommandLine.Model.CommandSpec spec;

    @CommandLine.Parameters(description = "LKQL files to refactor")
    public List<String> files = new ArrayList<>();

    @CommandLine.Option(names = { "-i", "--inplace" }, description = "Rewrite the files in place")
    public boolean inPlace;

    private enum RefactoringKind {
        IS_TO_COLON,
        TO_LKQL_V2,
    }

    @CommandLine.Option(
        names = { "-r", "--refactoring" },
        description = "Refactoring to run. Valid values: ${COMPLETION-CANDIDATES}",
        required = true
    )
    private RefactoringKind refactoring;

    /**
     * Return the refactoring corresponding to enum value passed on command line. This is where
     * concrete refactorings are implemented.
     */
    public Refactoring getRefactoring(AnalysisUnit unit) {
        return switch (refactoring) {
            case IS_TO_COLON -> (TokenBasedRefactoring) state -> {
                Refactoring.stream(state.unit.getRoot())
                    .filter(n -> n instanceof NodePatternDetail)
                    .forEach(det -> {
                        Refactoring.streamFrom(det.tokenStart())
                            .filter(t -> t.kind == TokenKind.LKQL_IS)
                            .findFirst()
                            .ifPresent(tokIs -> {
                                // Replace "is" -> ":"
                                state.replace(tokIs, ":");

                                // Get rid of previous token if it is a whitespace
                                var prev = tokIs.previous();
                                if (Refactoring.isWhitespace(prev)) {
                                    state.delete(prev);
                                }
                            });
                    });
            };
            case TO_LKQL_V2 -> new LKQLToLkt();
        };
    }

    /**
     * Rewrite a unit by printing refactored file, either to stdout, or to the
     * original file, depending on the value of the '-i' command line flag.
     */
    @Override
    public Integer call() {
        var ctx = AnalysisContext.create();
        for (var file : files) {
            var unit = ctx.getUnitFromFile(file);
            var result = getRefactoring(unit).apply(unit);

            if (inPlace) {
                try (var writer = new PrintWriter(unit.getFileName(), StandardCharsets.UTF_8)) {
                    writer.print(result);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            } else {
                System.out.print(result);
            }
        }
        return 0;
    }
}
