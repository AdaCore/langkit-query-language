//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import static com.adacore.liblkqllang.Liblkqllang.*;

import com.adacore.lkql_jit.options.Refactorings.LKQLToLkt;
import com.adacore.lkql_jit.options.Refactorings.Refactoring;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import picocli.CommandLine;

/**
 * Refactor command for LKQL. Allows to run automatic migrations, allowing the LKQL team to create
 * migrators to adapt to syntactic or semantic changes in LKQL.
 *
 * <p>Migrators work on the stream of tokens directly. Migrator implementer will attach actions
 * (append/prepend/replace) on tokens, which allows to modify the output stream without working on
 * text directly, which simplifies the expression of refactorings.
 *
 * <p>A migrator implementer will typically:
 *
 * <p>1. Find the nodes he wants to refactor in the LKQL tree 2. From those nodes, find the tokens
 * inside the nodes that need to be altered 3. Attach actions to those tokens
 *
 * <p>Then the rewriter will emit a new file for every LKQL unit, either inplace or on stdout.
 *
 * <p>To add a refactoring, you need to extend the 'refactoringKind' enum to add a new refactoring
 * id, and then extend the 'getRefactoring' method to return an anonymous function that will add
 * actions to the list of actions.
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

    private enum refactoringKind {
        IS_TO_COLON,
        TO_LKQL_V2,
    }

    @CommandLine.Option(
        names = { "-r", "--refactoring" },
        description = "Refactoring to run. Valid values: ${COMPLETION-CANDIDATES}",
        required = true
    )
    private refactoringKind refactoring;

    /**
     * Return the refactoring corresponding to enum value passed on command line. This is where
     * concrete refactorings are implemented.
     */
    public Refactoring getRefactoring(AnalysisUnit unit) {
        return switch (refactoring) {
            case IS_TO_COLON -> (Refactoring.State state) -> {
                for (var det : Refactoring.findAll(unit.getRoot(), n ->
                    n instanceof NodePatternDetail
                )) {
                    var tokIs = Refactoring.firstWithPred(
                        det.tokenStart(),
                        t -> t.kind == TokenKind.LKQL_IS
                    );

                    if (!tokIs.isNone()) {
                        // Replace "is" -> ":"
                        state.replace(tokIs, ":");

                        // Get rid of previous token if it is a whitespace
                        var prev = tokIs.previous();
                        if (Refactoring.isWhitespace(prev)) {
                            state.delete(tokIs.previous());
                        }
                    }
                }
            };
            case TO_LKQL_V2 -> LKQLToLkt.instantiate();
        };
    }

    @Override
    public Integer call() {
        var ctx = AnalysisContext.create();
        for (var file : files) {
            var unit = ctx.getUnitFromFile(file);
            var refactoring = getRefactoring(unit);
            var state = new Refactoring.State(unit);
            refactoring.applyRefactor(state);
            state.printAllTokens(this.inPlace);
        }
        return 0;
    }
}
