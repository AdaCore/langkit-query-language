//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options.Refactorings;

import static com.adacore.liblkqllang.Liblkqllang.Token.textRange;

import com.adacore.liblkqllang.Liblkqllang;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
            case Liblkqllang.ComplexPattern complexPattern -> refactorComplexPattern(
                complexPattern
            );
            case Liblkqllang.RecExpr recExpr -> refactorRecExpr(recExpr);
            case Liblkqllang.Query query -> refactorQuery(query);
            case Liblkqllang.ListComprehension comprehension -> refactorListComprehension(
                comprehension
            );
            case Liblkqllang.BlockBodyExpr bbe -> "val _ = " + refactorGeneric(bbe);
            case Liblkqllang.UnitLiteral _ -> "Unit()";
            case Liblkqllang.TopLevelList topLevel -> refactorTopLevelList(topLevel);
            case Liblkqllang.UniversalPattern _ -> "_";
            default -> refactorGeneric(node);
        };
    }

    /**
     * Copy all the text belonging to a node in the input source,
     * but recursively refactor the code of its children.
     * Ex:
     * - node = SomeNode(... # comment 1\n ... # comment 2\n ...)
     * - returns = "# comment 1\n#comment 2\n"
     *
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

    /**
     * Takes a node and returns the concatenation of all its comments
     * as a block of text.
     */
    private String getAllComments(Liblkqllang.LkqlNode node) {
        return Refactoring.streamFrom(node.tokenStart())
            .takeWhile(tok -> tok.tokenIndex < node.tokenEnd().tokenIndex)
            .filter(tok -> tok.isTrivia() && !tok.getText().isBlank())
            .map(tok -> tok.getText() + "\n")
            .collect(Collectors.joining());
    }

    private String refactorTopLevelList(Liblkqllang.TopLevelList topLevel) {
        var s = new StringBuilder();
        var cursor = topLevel.tokenStart();

        for (int i = 0; i < topLevel.getChildrenCount(); i++) {
            final var child = topLevel.getChild(i);
            if (child.isNone() || child.isGhost()) continue;
            // copy until child
            s.append(textRange(cursor, child.tokenStart().previous()));
            // copy child

            if (child instanceof Liblkqllang.Expr) {
                s.append("val _ = ");
            }
            s.append(refactorNode(child));
            // fast forward token cursor after child
            cursor = child.tokenEnd().next();
        }

        // copy until end
        s.append(textRange(cursor, topLevel.tokenEnd()));

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
     * rec( <left>,  <right>) --> <right> ::  <selector>(<left>)
     * rec(*<left>,  <right>) --> <right> ::  <left>.flat_map(<selector>)
     * rec( <left>, *<right>) --> <right> ::: <selector>(<left>)
     * rec(*<left>, *<right>) --> <right> ::: <left>.flat_map(<selector>)
     *
     */
    private String refactorRecExpr(Liblkqllang.RecExpr recExpr) {
        final var hasRight = !recExpr.fResultExpr().isNone();

        final var unpackLeft = recExpr.fRecurseUnpack().pAsBool();
        final var unpackRight = hasRight ? recExpr.fResultUnpack().pAsBool() : unpackLeft;

        final var left = recExpr.fRecurseExpr();
        final var right = hasRight ? recExpr.fResultExpr() : left;

        var s = unpackRight ? refactorNode(right) + " :::" : refactorNode(right) + " ::";

        // try to preserve spacing after "," (any newline for example)
        if (hasRight && left.tokenEnd().next().getText().equals(",")) {
            for (var tok = left.tokenEnd().next().next(); tok.isTrivia(); tok = tok.next()) s +=
                tok.getText();
        } else {
            s += " ";
        }

        s += unpackLeft
            ? refactorNode(left) + ".flat_map(" + currentSelector.fName().getText() + ")"
            : currentSelector.fName().getText() + "(" + refactorNode(left) + ")";

        return s;
    }

    /*
     * select <pattern>
     * from all_nodes match <pattern>
     *
     * from <expr> through <selector> select <pattern>
     * from <selector(expr)> match <pattern>
     *
     * Heuristics:
     * from <expr> through <selector> select <pattern>  (where <expr> is plural)
     * from <expr>.flat_map(<selector>) match <pattern>
     *
     * If first keyword:
     * from <expr> select first <pattern>
     * (from <expr> match <pattern>).head
     *
     */
    private String refactorQuery(Liblkqllang.Query query) {
        final var fromNode = query.fFromExpr();
        final var throughNode = query.fThroughExpr();

        final String source;

        if (fromNode.isNone()) {
            source = throughNode.isNone()
                ? "all_nodes"
                : "units().flat_map((unit) => " + refactorNode(throughNode) + "(unit.root))";
        } else {
            final var from = refactorNode(fromNode);
            final var through = throughNode.isNone() ? "children" : refactorNode(throughNode);

            // best effort heuristic to cover common cases
            final var isPlural =
                switch (fromNode) {
                    case Liblkqllang.ListLiteral _ -> true;
                    case Liblkqllang.ListComprehension _ -> true;
                    case Liblkqllang.DotAccess dot -> dot.fMember().getText().equals("children");
                    default -> false;
                };

            source = isPlural
                ? "(" + from + ").flat_map(" + through + ")"
                : through + "(" + from + ")";
        }

        var s = "from " + source + " match " + refactorNode(query.fPattern());

        if (query.fQueryKind() instanceof Liblkqllang.QueryKindFirst) {
            s = "(" + s + ").head";
        }

        return getAllComments(query) + s;
    }

    /*
     *
     * [ <expr> for <binding> in <source> if <guard> ]
     * from <source> match <binding> select <expr> if <guard>
     *
     * Multiple generators is handled as follow:
     * [ <expr> for <x_1> in <src_1>, ..., <x_n> in <src_n> if <guard> ]
     * <src_1>.flat_map(<x_1> => ... <src_n>.flat_map(<x_n> => if <guard> then [<expr>] else []))
     *
     */
    private String refactorListComprehension(Liblkqllang.ListComprehension comprehension) {
        final var hasGuard = !comprehension.fGuard().isNone();
        final var sb = new StringBuilder();

        final int nbSources = comprehension.fGenerators().getChildrenCount();

        final var generators = new ArrayList<Liblkqllang.ListCompAssoc>();
        comprehension.fGenerators().iterator().forEachRemaining(generators::add);

        sb.append(getAllComments(comprehension));

        // default case
        if (nbSources == 1) {
            sb.append("from ");
            sb.append(refactorNode(generators.get(0).fCollExpr()));
            sb.append(" match ");
            sb.append(refactorNode(generators.get(0).fBindingName()));
            sb.append(" select ");
            sb.append(refactorNode(comprehension.fExpr()));

            if (hasGuard) {
                sb.append(" if ");
                sb.append(refactorNode(comprehension.fGuard()));
            }
        }
        // special handling for multiple sources
        else {
            // open lambda for each source
            for (final var generator : comprehension.fGenerators()) {
                // simple heuristic to reduce parenthesis bloat
                if (generator.fCollExpr().isTokenNode()) {
                    sb.append(generator.fCollExpr().getText());
                } else {
                    sb.append("(");
                    sb.append(refactorNode(generator.fCollExpr()));
                    sb.append(")");
                }
                sb.append(".flat_map((");
                sb.append(refactorNode(generator.fBindingName()));
                sb.append(") => ");
            }

            if (hasGuard) {
                sb.append("if ");
                sb.append(refactorNode(comprehension.fGuard()));
                sb.append(" then ");
            }
            sb.append("[");
            sb.append(refactorNode(comprehension.fExpr()));
            sb.append("]");
            if (hasGuard) {
                sb.append(" else []");
            }

            // balance parenthesis, closing lambdas
            sb.repeat(')', nbSources);
        }

        return "(" + sb.toString() + ")";
    }

    /*
     * extrudes selectors sub-patterns into "when" clause
     */
    private String refactorComplexPattern(Liblkqllang.ComplexPattern complexPattern) {
        // Eliminate simple binding pattern
        if (complexPattern.fPattern().isNone()) return complexPattern.getText();

        // Collect detail patterns
        var selectorPatternDetails = new ArrayList<Liblkqllang.NodePatternSelector>();
        var otherPatternDetails = new ArrayList<Liblkqllang.NodePatternDetail>();
        for (var detail : complexPattern.fDetails()) {
            switch (detail) {
                case Liblkqllang.NodePatternSelector nps:
                    selectorPatternDetails.add(nps);
                    break;
                default:
                    otherPatternDetails.add(detail);
                    break;
            }
        }

        var sb = new StringBuilder();

        // Pattern binding
        if (!complexPattern.fBinding().isNone()) {
            // pattern has a binding
            sb.append(complexPattern.fBinding().getText());
            sb.append(" @ ");
        } else if (!selectorPatternDetails.isEmpty()) {
            // pattern has no binding but needs one
            sb.append("node @ ");
        }

        // Base pattern
        sb.append(refactorNode(complexPattern.fPattern()));

        // Pattern details
        if (!otherPatternDetails.isEmpty()) {
            sb.append("(");
            sb.append(
                otherPatternDetails
                    .stream()
                    .map(this::refactorNode)
                    .collect(Collectors.joining(", "))
            );
            sb.append(")");
        }

        // Predicate
        final var previousPredicate = complexPattern.fPredicate().isNone()
            ? Stream.<String>empty()
            : Stream.of(refactorNode(complexPattern.fPredicate()));
        final var newPredicates = selectorPatternDetails
            .stream()
            .map(this::refactorNodePatternSelector);
        final var predicates = Stream.concat(previousPredicate, newPredicates).collect(
            Collectors.joining(" and ")
        );
        if (!predicates.isEmpty()) {
            sb.append(" when ");
            sb.append(predicates);
        }

        return sb.toString();
    }

    /*
     * (<any|all> <selector>: <subpattern>)
     * <selector>(node).<any|all>((n) => n is <subpattern>)
     */
    private String refactorNodePatternSelector(Liblkqllang.NodePatternSelector nps) {
        final var quantifier = refactorNode(nps.fCall().fQuantifier());
        final var selector = refactorNode(nps.fCall().fSelectorCall());
        final var subPattern = refactorNode(nps.fPattern());

        final var name = "n";
        return (
            selector +
            "(node)." +
            quantifier +
            "((" +
            name +
            ") => " +
            name +
            " is " +
            subPattern +
            ")"
        );
    }
}
