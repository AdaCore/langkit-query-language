//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options.refactorings;

import com.adacore.liblkqllang.Liblkqllang;
import java.util.function.Consumer;
import java.util.stream.Stream;

/** Class offering an abstraction over all refactoring processes. */
@FunctionalInterface
public interface Refactoring {
    /** Take a parsing tree root and return its source after refactoring actions. */
    String apply(Liblkqllang.AnalysisUnit unit);

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
}
