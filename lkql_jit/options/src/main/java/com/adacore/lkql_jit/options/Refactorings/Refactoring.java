//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options.Refactorings;

import com.adacore.liblkqllang.Liblkqllang;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

public interface Refactoring {
    class State {

        public final Liblkqllang.AnalysisUnit unit;

        /** Kind of actions that can be attached to a token. */
        enum actionKind {
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
        record Action(actionKind kind, String text) {}

        /**
         * List of actions accumulated in this state object.
         */
        public final HashMap<String, List<Action>> actions = new HashMap<>();

        public State(Liblkqllang.AnalysisUnit unit) {
            this.unit = unit;
        }

        /** Add an action to the list of actions to apply */
        public void addAction(Liblkqllang.Token token, Action action) {
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

        public void delete(Liblkqllang.Token token) {
            addAction(token, new Action(actionKind.REPLACE, ""));
        }

        public void replace(Liblkqllang.Token token, String text) {
            addAction(token, new Action(actionKind.REPLACE, text));
        }

        public void append(Liblkqllang.Token token, String text) {
            addAction(token, new Action(actionKind.APPEND, text));
        }

        public void prepend(Liblkqllang.Token token, String text) {
            addAction(token, new Action(actionKind.PREPEND, text));
        }

        /**
         * Internal method: rewrite a unit by printing all the tokens via the 'write' callback. Apply
         * the actions at the same time.
         */
        private void printAllTokens(Consumer<String> write) {
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
         * Rewrite a unit by printing all the tokens, either to stdout, or to the
         * original file, depending on the value of the '-i' command line flag.
         */
        public void printAllTokens(boolean inPlace) {
            try {
                if (inPlace) {
                    var writer = new PrintWriter(unit.getFileName(), StandardCharsets.UTF_8);
                    printAllTokens(writer::print);
                    writer.close();
                } else {
                    printAllTokens(System.out::print);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        public String toString() {
            var writer = new StringWriter();
            printAllTokens(writer::write);
            return writer.toString();
        }
    }

    /**
     * Helper to create a unique Id for a token. TODO: This method exists only because the
     * 'hashCode' method on Tokens is wrong. This should be fixed at the Langkit level. See
     * eng/libadalang/langkit#857.
     *
     * @return the id as a string
     */
    private static String getTokenId(Liblkqllang.Token token) {
        return (
            token.unit.getFileName() +
            (token.isTrivia() ? "trivia " + token.triviaIndex : token.tokenIndex) +
            token.kind
        );
    }

    public static boolean isWhitespace(Liblkqllang.Token token) {
        return token.kind == Liblkqllang.TokenKind.LKQL_WHITESPACE;
    }

    /**
     * Helper for refactor writers: Returns the first token that satisfies the predicate 'pred',
     * iterating on tokens from 'fromTok' (including 'fromTok').
     *
     * <p>If no token is found, return the null token.
     */
    public static Liblkqllang.Token firstWithPred(
        Liblkqllang.Token fromTok,
        Predicate<Liblkqllang.Token> pred
    ) {
        var curTok = fromTok;
        while (!curTok.isNone() && !pred.test(curTok)) {
            curTok = curTok.next();
        }
        return curTok;
    }

    /**
     * Helper for findAll. Visit all children of 'node', calling 'cons' on each of them. TODO: Hoist
     * in Java bindings (see eng/libadalang/langkit#859)
     */
    private static void visitChildren(
        Liblkqllang.LkqlNode node,
        Consumer<Liblkqllang.LkqlNode> cons
    ) {
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
    static List<Liblkqllang.LkqlNode> findAll(
        Liblkqllang.LkqlNode root,
        Predicate<Liblkqllang.LkqlNode> pred
    ) {
        var result = new ArrayList<Liblkqllang.LkqlNode>();
        visitChildren(root, c -> {
            if (pred.test(c)) {
                result.add(c);
            }
        });
        return result;
    }

    void applyRefactor(State state);
}
