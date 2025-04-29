//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli;

import static com.adacore.liblkqllang.Liblkqllang.*;

import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Predicate;
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
    }

    @CommandLine.Option(
        names = { "-r", "--refactoring" },
        description = "Refactoring to run. Valid values: ${COMPLETION-CANDIDATES}",
        required = true
    )
    private refactoringKind refactoring;

    /** Kind of actions that can be attached to a token. */
    private enum actionKind {
        APPEND,
        PREPEND,
        REPLACE,
    }

    /**
     * Action record, encapsulating an action that can be attached to a token. When applied, the
     * action will either append the given text, prepend the given text, or replace the token text
     * with the given text, depending on the kind.
     *
     * <p>To remove a token, simply use a REPLACE action with an empty text.
     */
    private record Action(actionKind kind, String text) {}

    /**
     * Global map of actions that will be applied on tokens. To append an action, use the addAction
     * method.
     */
    private final HashMap<String, List<Action>> actions = new HashMap<>();

    /**
     * Helper to create a unique Id for a token. TODO: This method exists only because the
     * 'hashCode' method on Tokens is wrong. This should be fixed at the Langkit level. See
     * eng/libadalang/langkit#857.
     *
     * @return the id as a string
     */
    private String getTokenId(Token token) {
        return (
            token.unit.getFileName() +
            (token.isTrivia() ? "trivia " + token.triviaIndex : token.tokenIndex) +
            token.kind
        );
    }

    /** Add an action to the list of actions to apply */
    public void addAction(Token token, Action action) {
        List<Action> actionList;
        var tokenId = getTokenId(token);
        if (actions.containsKey(tokenId)) {
            actionList = actions.get(tokenId);
        } else {
            actionList = new ArrayList<>();
            actions.put(tokenId, actionList);
        }

        actionList.add(action);
    }

    /**
     * Helper for refactor writers: Returns the first token that satisfies the predicate 'pred',
     * iterating on tokens from 'fromTok' (including 'fromTok').
     *
     * <p>If no token is found, return the null token.
     */
    public static Token firstWithPred(Token fromTok, Predicate<Token> pred) {
        var curTok = fromTok;
        while (!curTok.isNone() && !pred.test(curTok)) {
            curTok = curTok.next();
        }
        return curTok;
    }

    /**
     * Internal method: rewrite a unit by printing all the tokens via the 'write' callback. Apply
     * the actions at the same time.
     */
    private void printAllTokens(AnalysisUnit unit, Consumer<String> write) {
        for (var tok = unit.getFirstToken(); !tok.isNone(); tok = tok.next()) {
            var tokActions = actions.get(getTokenId(tok));
            if (tokActions != null) {
                var replaceActions = tokActions
                    .stream()
                    .filter(c -> c.kind == actionKind.REPLACE)
                    .toList();
                assert replaceActions.size() <= 1 : "Only one replace action per token";

                var prependActions = tokActions
                    .stream()
                    .filter(c -> c.kind == actionKind.PREPEND)
                    .toList();

                var appendActions = tokActions
                    .stream()
                    .filter(c -> c.kind == actionKind.APPEND)
                    .toList();

                for (var action : prependActions) {
                    write.accept(action.text);
                }

                if (!replaceActions.isEmpty()) {
                    write.accept(replaceActions.get(0).text);
                } else {
                    write.accept(tok.getText());
                }

                for (var action : appendActions) {
                    write.accept(action.text);
                }
            } else {
                write.accept(tok.getText());
            }
        }
    }

    /**
     * Internal method: rewrite a unit by printing all the tokens, either to stdout, or to the
     * original file, depending on the value of the '-i' command line flag.
     */
    private void printAllTokens(AnalysisUnit unit) {
        try {
            if (this.inPlace) {
                var writer = new PrintWriter(unit.getFileName(), StandardCharsets.UTF_8);
                printAllTokens(unit, writer::print);
                writer.close();
            } else {
                printAllTokens(unit, System.out::print);
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Helper for findAll. Visit all children of 'node', calling 'cons' on each of them. TODO: Hoist
     * in Java bindings (see eng/libadalang/langkit#859)
     */
    private static void visitChildren(LkqlNode node, Consumer<LkqlNode> cons) {
        if (node == null || node.isNone()) {
            return;
        }

        for (var c : node.children()) {
            if (c != null && !c.isNone()) {
                cons.accept(c);
                visitChildren(c, cons);
            }
        }
    }

    /**
     * Helper for refactor writers: Find all nodes that are children of root and which satisfies the
     * predicate 'pred'. TODO: Hoist in Java bindings (see eng/libadalang/langkit#859)
     */
    public static List<LkqlNode> findAll(LkqlNode root, Predicate<LkqlNode> pred) {
        var result = new ArrayList<LkqlNode>();
        visitChildren(root, c -> {
            if (pred.test(c)) {
                result.add(c);
            }
        });
        return result;
    }

    /**
     * Return the refactoring corresponding to enum value passed on command line. This is where
     * concrete refactorings are implemented.
     */
    public Consumer<AnalysisUnit> getRefactoring() {
        switch (refactoring) {
            case IS_TO_COLON:
                return unit -> {
                    for (var det : findAll(unit.getRoot(), n -> n instanceof NodePatternDetail)) {
                        var tokIs = firstWithPred(
                            det.tokenStart(),
                            t -> t.kind == TokenKind.LKQL_IS
                        );

                        if (!tokIs.isNone()) {
                            // Replace "is" -> ":"
                            addAction(tokIs, new Action(actionKind.REPLACE, ":"));

                            // Get rid of previous token if it is a whitespace
                            var prev = tokIs.previous();
                            if (prev.kind == TokenKind.LKQL_WHITESPACE) {
                                addAction(tokIs.previous(), new Action(actionKind.REPLACE, ""));
                            }
                        }
                    }
                };
        }
        return null;
    }

    @Override
    public Integer call() {
        var refactoring = getRefactoring();
        var ctx = AnalysisContext.create();
        for (var file : files) {
            var unit = ctx.getUnitFromFile(file);
            refactoring.accept(unit);
            printAllTokens(unit);
            actions.clear();
        }
        return 0;
    }
}
