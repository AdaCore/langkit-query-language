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
import java.util.stream.Stream;

public interface Refactoring {
    class State {

        public final Liblkqllang.AnalysisUnit unit;

        /** Kind of actions that can be attached to a token. */
        enum ActionKind {
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
        private record Action(ActionKind kind, String text) {}

        /**
         * List of actions accumulated in this state object.
         */
        public final HashMap<String, List<Action>> actions = new HashMap<>();

        public State(Liblkqllang.AnalysisUnit unit) {
            this.unit = unit;
        }

        /** Add an action to the list of actions to apply */
        private void addAction(Liblkqllang.Token token, Action action) {
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
            addAction(token, new Action(ActionKind.REPLACE, ""));
        }

        public void replace(Liblkqllang.Token token, String text) {
            addAction(token, new Action(ActionKind.REPLACE, text));
        }

        public void replaceRange(Liblkqllang.Token start, Liblkqllang.Token end, String text) {
            var cur = start;
            while (cur.tokenIndex != end.tokenIndex) {
                delete(cur);
                cur = cur.next();
            }
            // append new code
            replace(end, text);
        }

        public void append(Liblkqllang.Token token, String text) {
            addAction(token, new Action(ActionKind.APPEND, text));
        }

        public void prepend(Liblkqllang.Token token, String text) {
            addAction(token, new Action(ActionKind.PREPEND, text));
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
                        .filter(c -> c.kind == ActionKind.REPLACE)
                        .toList();
                    assert replaceActions.size() <= 1 : "Only one replace action per token";

                    var prependActions = tokActions
                        .stream()
                        .filter(c -> c.kind == ActionKind.PREPEND)
                        .toList();

                    var appendActions = tokActions
                        .stream()
                        .filter(c -> c.kind == ActionKind.APPEND)
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
     * Internal helper for stream method.
     * Visit all children of 'node', calling 'cons' on each of them.
     * TODO: Hoist in Java bindings (see eng/libadalang/langkit#859)
     */
    private static void visitChildren(
        Liblkqllang.LkqlNode node,
        Consumer<Liblkqllang.LkqlNode> cons
    ) {
        if (node != null && !node.isNone()) {
            cons.accept(node);
            for (var child : node.children()) visitChildren(child, cons);
        }
    }

    /** Creates a node stream visiting all children of the input node. */
    public static Stream<Liblkqllang.LkqlNode> stream(Liblkqllang.LkqlNode root) {
        final var b = Stream.<Liblkqllang.LkqlNode>builder();
        visitChildren(root, b);
        return b.build();
    }

    /** Creates a token stream starting at the input token. */
    public static Stream<Liblkqllang.Token> streamFrom(Liblkqllang.Token start) {
        return Stream.iterate(start, t -> !t.isNone(), t -> t.next());
    }

    void applyRefactor(State state);
}
