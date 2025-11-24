//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options.Refactorings;

import static com.adacore.liblkqllang.Liblkqllang.Token.textRange;

import com.adacore.liblkqllang.Liblkqllang;

public class LKQLToLkt implements TreeBasedRefactoring {

    /** Pointer to last-entered selector during rewriting. */
    private Liblkqllang.SelectorDecl currentSelector = Liblkqllang.SelectorDecl.NONE;

    @Override
    public String apply(Liblkqllang.AnalysisUnit unit) {
        var root = unit.getRoot();
        return (
            "# lkql version: 2\n\n" +
            textRange(unit.getFirstToken(), root.tokenStart().previous()) +
            apply(root) +
            textRange(root.tokenEnd().next(), unit.getLastToken())
        );
    }

    public String apply(Liblkqllang.LkqlNode root) {
        return refactorNode(root);
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
            case Liblkqllang.SelectorArm arm -> refactorArm(arm, arm.fPattern(), arm.fExpr());
            case Liblkqllang.SelectorDecl selectorDecl -> refactorSelectorDecl(selectorDecl);
            case Liblkqllang.RecExpr recExpr -> refactorRecExpr(recExpr);
            case Liblkqllang.BlockBodyExpr bbe -> "val _ = " + refactorGeneric(bbe);
            case Liblkqllang.UnitLiteral _ -> "Unit()";
            case Liblkqllang.Expr expr when (
                expr.parent() instanceof Liblkqllang.TopLevelList
            ) -> "val _ = " + refactorGeneric(expr);
            case Liblkqllang.UniversalPattern _ -> "_";
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

    /*
     *
     * <annotations> selector <name> <docstring> <arms>
     *
     * <docstring>\n
     * <annotations> fun <name> (this : Any) : Any = match this { <arms> }
     *
     */
    private String refactorSelectorDecl(Liblkqllang.SelectorDecl selectorDecl) {
        // save selector state on entering this function
        var previousSelector = currentSelector;
        // set new state for nested refactors
        currentSelector = selectorDecl;
        var s = "";

        // pull docstring before declaration
        if (!selectorDecl.fDocNode().isNone()) s = refactorNode(selectorDecl.fDocNode()) + "\n";

        if (!selectorDecl.fAnnotation().isNone()) s +=
            refactorNode(selectorDecl.fAnnotation()) +
            selectorDecl.fAnnotation().tokenEnd().next().getText();

        final var whitespace = textRange(
            (selectorDecl.fDocNode().isNone()
                    ? selectorDecl.fName().tokenEnd().next()
                    : selectorDecl.fDocNode().tokenEnd().next()),
            selectorDecl.fArms().tokenStart().previous()
        );

        s +=
            "fun " +
            refactorNode(selectorDecl.fName()) +
            "(this : Any) : Any = match this {" +
            whitespace +
            refactorNode(selectorDecl.fArms()) +
            "\n}\n";

        // restore previous state on exiting this function
        currentSelector = previousSelector;
        return s;
    }

    /*
     *
     * 1) Expansion of implicit argument
     *
     * rec(<expr>) --> rec(<expr>, <expr>)
     *
     * 2) Case disjonction
     *
     * rec( <left>,  <right>) --> <right>          ::  <selector>(<left>)
     * rec(*<left>,  <right>) --> <right>          ::  <left>.iterator.flatMap(<selector>)
     * rec( <left>, *<right>) --> <right>.iterator ::: <selector>(<left>)
     * rec(*<left>, *<right>) --> <right>.iterator ::: <left>.iterator.flatMap(<selector>)
     *
     */
    private String refactorRecExpr(Liblkqllang.RecExpr recExpr) {
        final var hasRight = !recExpr.fResultExpr().isNone();

        final var unpackLeft = recExpr.fRecurseUnpack().pAsBool();
        final var unpackRight = hasRight ? recExpr.fResultUnpack().pAsBool() : unpackLeft;

        final var left = recExpr.fRecurseExpr();
        final var right = hasRight ? recExpr.fResultExpr() : left;

        var s = unpackRight ? refactorNode(right) + ".iterator :::" : refactorNode(right) + " ::";

        // try to preserve spacing after "," (any newline for example)
        if (hasRight && left.tokenEnd().next().getText().equals(",")) {
            for (var tok = left.tokenEnd().next().next(); tok.isTrivia(); tok = tok.next()) s +=
                tok.getText();
        } else {
            s += " ";
        }

        s += unpackLeft
            ? refactorNode(left) + ".iterator.flatMap(" + currentSelector.fName().getText() + ")"
            : currentSelector.fName().getText() + "(" + refactorNode(left) + ")";

        return s;
    }
}
