//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.refactorings;

import static com.adacore.liblkqllang.Liblkqllang.Token.textRange;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.liblkqllang.Liblkqllang.TokenKind;
import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.variants.Warning;
import com.adacore.lkql_jit.driver.source_support.SourceLinesCache;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.ArrayList;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LKQLToLkt implements TreeBasedRefactoring {

    /** Pointer to last-entered selector during rewriting. */
    private Liblkqllang.SelectorDecl currentSelector = Liblkqllang.SelectorDecl.NONE;

    private DiagnosticCollector diags;

    private SourceLinesCache cache;

    @Override
    public String apply(
        Liblkqllang.AnalysisUnit unit,
        DiagnosticCollector diags,
        SourceLinesCache cache
    ) {
        this.diags = diags;
        this.cache = cache;
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
            case Liblkqllang.FunCall funCall -> refactorFunCall(funCall);
            case Liblkqllang.ParameterDecl paramDecl -> refactorParamDecl(paramDecl);
            case Liblkqllang.InClause inClause -> refactorInClause(inClause);
            case Liblkqllang.Match match -> refactorMatch(match);
            case Liblkqllang.MatchArm arm -> refactorArm(arm, arm.fPattern(), arm.fExpr());
            case Liblkqllang.SelectorArm arm -> refactorSelectorArm(
                arm,
                arm.fPattern(),
                arm.fExpr()
            );
            case Liblkqllang.SelectorDecl selectorDecl -> refactorSelectorDecl(selectorDecl);
            case Liblkqllang.ComplexPattern complexPattern -> refactorComplexPattern(
                complexPattern
            );
            case Liblkqllang.RecExpr recExpr -> refactorRecExpr(recExpr);
            case Liblkqllang.Query query -> refactorQuery(query);
            case Liblkqllang.ListComprehension comprehension -> refactorListComprehension(
                comprehension
            );
            case Liblkqllang.Indexing indexing -> refactorIndexing(indexing);
            case Liblkqllang.Tuple tuple -> refactorTuple(tuple);
            case Liblkqllang.TuplePattern tuplePattern -> refactorTuplePattern(tuplePattern);
            case Liblkqllang.ConstructorCall consCall -> refactorConstructorCall(consCall);
            case Liblkqllang.ObjectLiteral objLit -> refactorObjectLiteral(objLit);
            case Liblkqllang.CondExpr condExpr -> refactorGeneric(condExpr) +
            (condExpr.fElseExpr().isNone() ? " else true" : "");
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
     * Ex:
     * - node = SomeNode(... # comment 1\n ... # comment 2\n ...)
     * - returns = "# comment 1\n#comment 2\n"
     *
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

            if (i == 0 && child instanceof Liblkqllang.BlockStringLiteral doc) {
                // module doc
                s.append("|\"\"");
                s.append(doc.getText().replace("\n|\"", "\n|\"\"").substring(2));
            } else {
                if (child instanceof Liblkqllang.Expr) {
                    s.append("val _ = ");
                }
                s.append(refactorNode(child));
            }

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
        var sb = new StringBuilder();
        sb.append(
            textRange(
                namedFunction.tokenStart(),
                namedFunction.fParameters().tokenStart().previous()
            )
        );

        sb.append(refactorNode(namedFunction.fParameters()));

        sb.append(") : Any");

        var cursor = namedFunction.fParameters().tokenEnd().next();
        if (cursor.getText().equals(")")) cursor = cursor.next();

        while (!cursor.getText().equals("=")) {
            sb.append(cursor.getText());
            cursor = cursor.next();
        }

        sb.append("=");

        if (!namedFunction.fDocNode().isNone()) {
            sb.append(textRange(cursor.next(), namedFunction.fDocNode().tokenStart().previous()));
            sb.append(
                textRange(
                    namedFunction.fDocNode().tokenEnd().next(),
                    namedFunction.fBodyExpr().tokenStart().previous()
                )
            );
        } else {
            sb.append(textRange(cursor.next(), namedFunction.fBodyExpr().tokenStart().previous()));
        }

        sb.append(refactorNode(namedFunction.fBodyExpr()));
        return sb.toString();
    }

    /*
     *
     * <callee>[?](<args>)
     * <callee>(<args>)
     *
     */
    private String refactorFunCall(Liblkqllang.FunCall funCall) {
        if (funCall.fHasSafe().pAsBool()) {
            diags.add(
                new Warning(
                    "safe calls are a deprecated feature",
                    SourceSection.wrap(funCall.fHasSafe(), cache)
                )
            );
        }

        final var sb = new StringBuilder();

        sb.append(refactorNode(funCall.fName()));

        var cursor = funCall.fName().tokenEnd().next();
        var stopIndex = funCall.fArguments().tokenStart().tokenIndex;
        while (cursor.tokenIndex < stopIndex) {
            if (cursor.kind != TokenKind.LKQL_QUESTION) {
                sb.append(cursor.getText());
            }
            cursor = cursor.next();
        }

        sb.append(refactorNode(funCall.fArguments()));

        if (funCall.fArguments().tokenEnd().next().tokenIndex < funCall.tokenEnd().tokenIndex) {
            sb.append(textRange(funCall.fArguments().tokenEnd().next(), funCall.tokenEnd()));
        } else {
            sb.append(textRange(funCall.tokenEnd(), funCall.tokenEnd()));
        }

        return sb.toString();
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
     * | <pattern> => <expr>
     *
     * case <pattern> => <expr>
     *
     * INFO special handling if <expr> is a UnitLiteral,
     * as this has a special meaning for selectors
     *
     */
    private String refactorSelectorArm(
        Liblkqllang.LkqlNode arm,
        Liblkqllang.BasePattern pattern,
        Liblkqllang.Expr expr
    ) {
        String refactoredExpr = expr instanceof Liblkqllang.UnitLiteral
            ? "Rec([], [])"
            : refactorNode(expr);
        return (
            "case" +
            textRange(arm.tokenStart().next(), pattern.tokenStart().previous()) +
            refactorNode(pattern) +
            textRange(pattern.tokenEnd().next(), expr.tokenStart().previous()) +
            refactoredExpr +
            textRange(expr.tokenEnd().next(), arm.tokenEnd())
        );
    }

    /*
     *
     * <annotations> selector <name> <docstring> <arms>
     *
     * fun <name>_body (this : Any) : Any = match this { <arms> }
     *
     * <docstring>\n
     * <annotations> fun <name> (
     *     this : Any,
     *     depth : Int = -1,
     *     min_depth : Int = -1,
     *     max_depth : Int = -1
     * ) : Any = unfold(<name>_body,
     *                  make_depth_predicate(depth, min_depth, max_depth),
     *                  [this],
     *                  1)
     *
     */
    private String refactorSelectorDecl(Liblkqllang.SelectorDecl selectorDecl) {
        // save selector state on entering this function
        var previousSelector = currentSelector;
        // set new state for nested refactors
        currentSelector = selectorDecl;

        final var name = refactorNode(selectorDecl.fName());
        final var name_body = name + "_body";

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

        final var isMemoized =
            !selectorDecl.fAnnotation().isNone() &&
            selectorDecl.fAnnotation().fName().getText().equals(Constants.ANNOTATION_MEMOIZED);

        final var unfold = isMemoized ? "unfold_check_cycles" : "unfold";

        final var optionalLastArg = isMemoized ? ", []" : "";

        s +=
            "fun " +
            name +
            "(this : Any, depth : Int = -1, min_depth : Int = -1, max_depth : Int = -1) : Any =\n    " +
            unfold +
            "(" +
            name_body +
            ", make_depth_predicate(depth, min_depth, max_depth), [this], 1" +
            optionalLastArg +
            ")";

        // Handle selectors defined in blocks
        if (!(selectorDecl.parent() instanceof Liblkqllang.TopLevelList)) s += ";";

        s +=
            "\n\nfun " +
            name_body +
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
     * rec( <left>,  <right>) --> Rec([<right>], [<left>])
     * rec(*<left>,  <right>) --> Rec( <right> , [<left>])
     * rec( <left>, *<right>) --> Rec([<right>],  <left> )
     * rec(*<left>, *<right>) --> Rec( <right> ,  <left> )
     *
     */
    private String refactorRecExpr(Liblkqllang.RecExpr recExpr) {
        final var hasRight = !recExpr.fResultExpr().isNone();

        final var unpackLeft = recExpr.fRecurseUnpack().pAsBool();
        final var unpackRight = hasRight ? recExpr.fResultUnpack().pAsBool() : unpackLeft;

        final var left = recExpr.fRecurseExpr();
        final var right = hasRight ? recExpr.fResultExpr() : left;

        // wrap in prelude-defined function for runtime support
        final Function<String, String> wrapper = s -> "non_null(" + s + ")";

        var s = unpackRight ? refactorNode(right) : wrapper.apply(refactorNode(right));

        s += ",";

        // try to preserve spacing after "," (any newline for example)
        if (hasRight && left.tokenEnd().next().getText().equals(",")) {
            for (var tok = left.tokenEnd().next().next(); tok.isTrivia(); tok = tok.next()) {
                s += tok.getText();
            }
        } else {
            s += " ";
        }

        s += unpackLeft ? refactorNode(left) : wrapper.apply(refactorNode(left));

        return "Rec(" + s + ")";
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
     * (from <expr> match <pattern>).head_or(null)
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
            final var through = throughNode.isNone() ? "subtree" : refactorNode(throughNode);

            // best effort heuristic to cover common cases
            final var isPlural = switch (fromNode) {
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
            s = "(" + s + ").head_or(null)";
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

        var hasBinding = true;

        // Pattern binding
        if (!complexPattern.fBinding().isNone()) {
            // pattern has a binding
            sb.append(complexPattern.fBinding().getText());
        } else if (!selectorPatternDetails.isEmpty()) {
            // pattern has no binding but needs one
            sb.append("node");
        } else {
            hasBinding = false;
        }

        final var isUniv = complexPattern.fPattern() instanceof Liblkqllang.UniversalPattern;
        final var hasDetails = !otherPatternDetails.isEmpty();

        if ((hasBinding && hasDetails) || (hasBinding && !isUniv)) {
            sb.append(" @ ");
        }

        if (isUniv) {
            if (hasDetails) {
                sb.append("AdaNode");
            } else if (!hasBinding) {
                sb.append("_");
            }
        } else {
            // Base pattern
            sb.append(refactorNode(complexPattern.fPattern()));
        }

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
     *
     * (<any|all> <selector>(<args>): <subpattern>)
     * <selector>(node, <args>).<any|all>((n) => n is <subpattern>)
     *
     */
    private String refactorNodePatternSelector(Liblkqllang.NodePatternSelector nps) {
        final var quantifier = refactorNode(nps.fCall().fQuantifier());
        final var selector = refactorNode(nps.fCall().fSelectorCall());
        final var subPattern = refactorNode(nps.fPattern());

        final var matcher = Pattern.compile("(\\w+)(\\((.*)\\))?").matcher(selector);
        matcher.matches();
        final var selectorName = matcher.group(1);
        final var selectorArgs = matcher.group(3);

        final var name = "n";
        return (
            selectorName +
            "(node" +
            (selectorArgs != null ? ", " + selectorArgs : "") +
            ")." +
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

    /*
     * <obj><issafe>[<idx>]
     *
     * <obj>?.at(<idx>-1) if issafe
     * <obj>?[<idx>-1] otherwise
     *
     * NB: Since LKQL V1 is null-safe by default, the refactor always produce
     * null-safe Lkt variants
     *
     */
    private String refactorIndexing(Liblkqllang.Indexing indexing) {
        final String obj = refactorNode(indexing.fCollectionExpr());
        final boolean isSafe = indexing instanceof Liblkqllang.SafeIndexing;
        final String idx;
        if (indexing.fIndexExpr() instanceof Liblkqllang.IntegerLiteral lit) {
            long value = Long.parseLong(lit.getText());
            idx = Long.toString(value - 1);
        } else {
            idx = "(" + refactorNode(indexing.fIndexExpr()) + ")-1";
        }
        return isSafe ? obj + "?.at(" + idx + ")" : obj + "?[" + idx + "]";
    }

    /*
     * (<a>, <b>)
     * Pair(<a>, <b>)
     */
    private String refactorTuple(Liblkqllang.Tuple tuple) {
        final var s = refactorGeneric(tuple);
        if (tuple.fExprs().getChildrenCount() > 2) {
            diags.add(
                new Warning(
                    "tuples of more than 2 elements cannot be refactored automatically, consider introducing a new struct type",
                    SourceSection.wrap(tuple, cache)
                )
            );
            return s;
        }

        return "Pair" + s;
    }

    /*
     * case (<a>, <b>) =>
     * case Pair(<a>, <b>) =>
     */
    private String refactorTuplePattern(Liblkqllang.TuplePattern tuplePattern) {
        if (tuplePattern.fPatterns().getChildrenCount() > 2) {
            diags.add(
                new Warning(
                    "tuples patterns of more than 2 elements cannot be refactored automatically",
                    SourceSection.wrap(tuplePattern, cache)
                )
            );
            return refactorGeneric(tuplePattern);
        }

        final var fst = refactorNode(tuplePattern.fPatterns().getChild(0));
        final var snd = refactorNode(tuplePattern.fPatterns().getChild(1));

        return "Pair(fst: " + fst + ", snd: " + snd + ")";
    }

    private String refactorConstructorCall(Liblkqllang.ConstructorCall consCall) {
        final var lpar = textRange(
            consCall.fName().tokenEnd().next(),
            consCall.fArguments().tokenStart().previous()
        );
        final var rpar =
            textRange(consCall.fArguments().tokenEnd().next(), consCall.tokenEnd().previous()) +
            ")";
        return refactorNode(consCall.fName()) + lpar + refactorNode(consCall.fArguments()) + rpar;
    }

    private String refactorObjectLiteral(Liblkqllang.ObjectLiteral objLit) {
        diags.add(
            new Warning(
                "objects literals cannot be refactored automatically, consider introducing a new struct type",
                SourceSection.wrap(objLit, cache)
            )
        );
        return refactorGeneric(objLit);
    }

    /*
     * <a> in <b>
     * { val _tmp = <a>; (<b>).any((b) => _tmp == b) }
     */
    private String refactorInClause(Liblkqllang.InClause inClause) {
        return (
            "{ val _tmp = " +
            refactorNode(inClause.fValueExpr()) +
            "; (" +
            refactorNode(inClause.fListExpr()) +
            ").any((b) => _tmp == b) }"
        );
    }
}
