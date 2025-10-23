//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options.Refactorings;

import com.adacore.liblkqllang.Liblkqllang;

public class LKQLToLkt {

    public static Refactoring instantiate() {
        return (Refactoring.State state) -> {
            state.prepend(state.unit.getRoot().tokenStart(), "# lkql version: 2\n\n");

            // We need to handle:
            for (var fd : Refactoring.findAll(state.unit.getRoot(), n ->
                n instanceof Liblkqllang.NamedFunction
            )) {
                var namedFun = (Liblkqllang.NamedFunction) fd;

                // Docstrings: Remove the nested docstring and put it before the function
                // declaration
                var doc = namedFun.fDocNode();
                if (!doc.isNone()) {
                    var tok = doc.tokenStart();
                    var afterEnd = doc.tokenEnd().next();
                    while (!tok.equals(afterEnd)) {
                        state.delete(tok);
                        tok = tok.next();
                    }

                    while (Refactoring.isWhitespace(afterEnd)) {
                        state.delete(afterEnd);
                        afterEnd = afterEnd.next();
                    }

                    var funDecl = (Liblkqllang.FunDecl) fd.parent();
                    state.prepend(funDecl.tokenStart(), doc.getText() + "\n");
                }

                // Type annotations: Add "Any" type annotations in every place where a type
                // is mandatory in Lkt, so mainly in named function declarations (everything
                // else is supposedly inferred).
                for (var p : namedFun.fParameters().children()) {
                    var param = (Liblkqllang.ParameterDecl) p;
                    state.append(param.fParamIdentifier().tokenEnd(), ": Any");
                }

                state.append(namedFun.fParameters().tokenEnd().next(), ": Any");
            }

            // Match expressions
            for (var me : Refactoring.findAll(state.unit.getRoot(), n ->
                n instanceof Liblkqllang.Match
            )) {
                var matchExpr = (Liblkqllang.Match) me;
                state.append(matchExpr.fMatchedVal().tokenEnd(), " {");

                for (var ma : matchExpr.fArms().children()) {
                    var matchArm = (Liblkqllang.MatchArm) ma;
                    state.replace(matchArm.tokenStart(), "case ");
                }

                state.append(matchExpr.tokenEnd(), "\n}");
            }

            // Naked expressions in decl blocks
            for (var bbe : Refactoring.findAll(state.unit.getRoot(), n ->
                n instanceof Liblkqllang.BlockBodyExpr
            )) {
                var blockBodyExpr = (Liblkqllang.BlockBodyExpr) bbe;

                state.prepend(blockBodyExpr.tokenStart(), "var _ = ");
            }

            // Naked expressions in the top-level
            for (var e : Refactoring.findAll(
                state.unit.getRoot(),
                n -> n instanceof Liblkqllang.Expr && n.parent() instanceof Liblkqllang.TopLevelList
            )) {
                var expr = (Liblkqllang.Expr) e;

                state.prepend(expr.tokenStart(), "val _ = ");
            }
        };
    }
}
