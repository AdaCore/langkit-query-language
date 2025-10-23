//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options.Refactorings;

import static com.adacore.liblkqllang.Liblkqllang.Token.textRange;

import com.adacore.liblkqllang.Liblkqllang;

public class LKQLToLkt implements Refactoring {

    @Override
    public void applyRefactor(Refactoring.State state) {
        final var root = state.unit.getRoot();
        state.prepend(root.tokenStart(), "# lkql version: 2\n\n");

        // Because this rewriting doesn't use the token/action API, this is a hack
        state.replaceRange(root.tokenStart(), root.tokenEnd(), refactorNode(root));
    }

    /** Computes the text representing the refactored node. */
    private String refactorNode(Liblkqllang.LkqlNode node) {
        if (node.isNone() || node.isGhost()) return "";

        return switch (node) {
            case Liblkqllang.FunDecl funDecl -> refactorFunDecl(funDecl);
            case Liblkqllang.NamedFunction namedFunction -> refactorNamedFunction(namedFunction);
            case Liblkqllang.ParameterDecl paramDecl -> refactorParamDecl(paramDecl);
            case Liblkqllang.Match match -> refactorMatch(match);
            case Liblkqllang.MatchArm arm -> refactorArm(arm, arm.fPattern(), arm.fExpr());
            case Liblkqllang.SelectorDecl selectorDecl -> refactorSelectorDecl(selectorDecl);
            case Liblkqllang.BlockBodyExpr bbe -> "var _ = " + refactorGeneric(bbe);
            case Liblkqllang.Expr expr when (
                expr.parent() instanceof Liblkqllang.TopLevelList
            ) -> "val _ = " + refactorGeneric(expr);
            default -> refactorGeneric(node);
        };
    }

    /**
     * Copy all the text belonging to a node in the input source,
     * but recursively refactor the code of its children.
     */
    private String refactorGeneric(Liblkqllang.LkqlNode node) {
        if (node.isTokenNode()) return node.getText();
        var s = new StringBuilder();
        var cursor = node.tokenStart();

        for (int i = 0; i < node.getChildrenCount(); i++) {
            final var child = node.getChild(i);
            if (child.isNone() || child.isGhost()) continue;
            // copy until child
            s.append(textRange(cursor, child.tokenStart().previous()));
            // copy child
            s.append(refactorNode(child));
            // fast forward token cursor after child
            cursor = child.tokenEnd().next();
        }

        // copy until end
        s.append(textRange(cursor, node.tokenEnd()));

        return s.toString();
    }

    /*
     *
     * fun <name> <funexpr>
     *
     * <docstring>\n
     * fun <name> <funexpr>
     *
     */
    private String refactorFunDecl(Liblkqllang.FunDecl funDecl) {
        var s =
            textRange(funDecl.tokenStart(), funDecl.fFunExpr().tokenStart().previous()) +
            refactorNode(funDecl.fFunExpr());

        // pull docstring out of function declaration
        var docstring = funDecl.fFunExpr().fDocNode();
        if (!docstring.isNone()) s = refactorNode(docstring) + "\n" + s;

        return s;
    }

    /*
     *
     * (<params>) = <docstring> <body>
     *
     * (<params>) : Any = <body>
     *
     */
    private String refactorNamedFunction(Liblkqllang.NamedFunction namedFunction) {
        var s =
            textRange(
                namedFunction.tokenStart(),
                namedFunction.fParameters().tokenStart().previous()
            ) +
            refactorNode(namedFunction.fParameters());

        var cursor = namedFunction.fParameters().tokenEnd().next();
        while (!cursor.getText().equals("=")) {
            s += cursor.getText();
            cursor = cursor.next();
        }

        s +=
            ": Any " +
            textRange(cursor, namedFunction.fBodyExpr().tokenStart().previous()) +
            refactorNode(namedFunction.fBodyExpr());
        return s;
    }

    /*
     *
     * <id> [: <type>] [= <expr>]
     *
     * <id> : (Any|<type>) [= expr]
     *
     */
    private String refactorParamDecl(Liblkqllang.ParameterDecl paramDecl) {
        var s = refactorNode(paramDecl.fParamIdentifier());

        var cursor = paramDecl.fParamIdentifier().tokenEnd().next();

        if (!paramDecl.fTypeAnnotation().isNone()) {
            s +=
                textRange(cursor, paramDecl.fTypeAnnotation().tokenStart().previous()) +
                refactorNode(paramDecl.fTypeAnnotation());
            cursor = paramDecl.fTypeAnnotation().tokenEnd().next();
        } else {
            s += " : Any"; // add type annotation if none
        }

        if (!paramDecl.fDefaultExpr().isNone()) {
            s +=
                textRange(cursor, paramDecl.fDefaultExpr().tokenStart().previous()) +
                refactorNode(paramDecl.fDefaultExpr());
        }

        return s;
    }

    /*
     *
     * match <expr> <arms>
     *
     * match <expr> { <arms> }
     *
     */
    private String refactorMatch(Liblkqllang.Match match) {
        return (
            textRange(match.tokenStart(), match.fMatchedVal().tokenStart().previous()) +
            refactorNode(match.fMatchedVal()) +
            " {" +
            textRange(
                match.fMatchedVal().tokenEnd().next(),
                match.fArms().tokenStart().previous()
            ) +
            refactorNode(match.fArms()) +
            "\n}\n"
        );
    }

    /*
     *
     * | <pattern> => <expr>
     *
     * case <pattern> => <expr>
     *
     */
    private String refactorArm(
        Liblkqllang.LkqlNode arm,
        Liblkqllang.BasePattern pattern,
        Liblkqllang.Expr expr
    ) {
        return (
            "case" +
            textRange(arm.tokenStart().next(), pattern.tokenStart().previous()) +
            refactorNode(pattern) +
            textRange(pattern.tokenEnd().next(), expr.tokenStart().previous()) +
            refactorNode(expr) +
            textRange(expr.tokenEnd().next(), arm.tokenEnd())
        );
    }
}
