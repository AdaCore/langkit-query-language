//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.drivers;

import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Predicate;

import picocli.CommandLine;
import static com.adacore.liblkqllang.Liblkqllang.*;

@CommandLine.Command(
        name = "refactor",
        description = "Automatically run a refactoring on LKQL code")
public class LKQLRefactor implements Callable<Integer> {

    enum actionKind {
        APPEND,
        PREPEND,
        REPLACE
    }

    class Action {
        actionKind kind;
        String text;
    }

    HashMap<Token, List<Action>> actions = new HashMap<>();

    public static Token firstWithPred(Token fromTok, Predicate<Token> pred) {
        var curTok = fromTok;
        while (!pred.test(fromTok)) {
            curTok = curTok.next();
        }
        return curTok;
    }

    public void printAllTokens(AnalysisUnit unit, Consumer<String> write) {
        for (var tok = unit.getFirstToken(); tok != null; tok = tok.next()) {
            var tokActions = actions.get(tok);
            if (tokActions != null) {
                var replaceActions =
                    tokActions.stream().filter(c -> c.kind == actionKind.REPLACE).toList();
                assert replaceActions.size() <= 1 : "Only one replace action per token";

                var prependActions =
                    tokActions.stream().filter(c -> c.kind == actionKind.PREPEND).toList();

                var appendActions =
                    tokActions.stream().filter(c -> c.kind == actionKind.APPEND).toList();

                for (var action : prependActions) {
                    write.accept(action.text);
                }

                if (replaceActions.size() > 0) {
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

    public void printAllTokens(AnalysisUnit unit) {
        try {
            var writer = new PrintWriter(unit.getFileName(), StandardCharsets.UTF_8);

            if (this.inPlace) {
                printAllTokens(unit, writer::print);
            } else {
                printAllTokens(unit, System.out::print);
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @CommandLine.Spec public picocli.CommandLine.Model.CommandSpec spec;

    @CommandLine.Parameters(description = "LKQL files to refactor")
    public List<String> files = new ArrayList<>();

    @CommandLine.Option(names = {"-i", "--inplace"}, description = "Rewrite the files in place")
    public boolean inPlace;
    @CommandLine.Option(
            names = {"-r", "--refactoring"},
            description = "Refactoring to run")
    public String refactoringName;

    HashMap<String, Consumer<AnalysisUnit>> refactorings;

    @Override
    public Integer call() {
        refactorings.put("pattern_arg_is_to_colon", unit -> {
            for (var det : unit.getRoot().)
        });
        return 0;
    }
}
