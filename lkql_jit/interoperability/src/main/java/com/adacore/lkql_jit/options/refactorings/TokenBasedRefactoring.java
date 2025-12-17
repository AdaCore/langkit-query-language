//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options.refactorings;

import com.adacore.liblkqllang.Liblkqllang;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

@FunctionalInterface
public interface TokenBasedRefactoring extends Refactoring {
    @Override
    default String apply(Liblkqllang.AnalysisUnit unit) {
        var state = new State(unit);
        apply(state);
        return state.getRewroteUnit();
    }

    void apply(State state);

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

        /**
         * Helper to create a unique Id for a token. TODO: This method exists only because the
         * 'hashCode' method on Tokens is wrong. This should be fixed at the Langkit level. See
         * eng/libadalang/langkit#857.
         *
         * @return the id as a string
         */
        private String getTokenId(Liblkqllang.Token token) {
            return (
                token.unit.getFileName() +
                (token.isTrivia() ? "trivia " + token.triviaIndex : token.tokenIndex) +
                token.kind
            );
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
         * Rewrite a unit by printing all the tokens,
         * applying the actions at the same time.
         */
        private String getRewroteUnit() {
            var buffer = new StringBuilder();
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
                        buffer.append(action.text);
                    }

                    if (!replaceActions.isEmpty()) {
                        buffer.append(replaceActions.get(0).text);
                    } else {
                        buffer.append(tok.getText());
                    }

                    for (var action : appendActions) {
                        buffer.append(action.text);
                    }
                } else {
                    buffer.append(tok.getText());
                }
            }
            return buffer.toString();
        }
    }
}
