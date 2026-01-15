//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.cli.refactorings;

import static com.adacore.liblkqllang.Liblkqllang.Token.textRange;

import com.adacore.liblkqllang.Liblkqllang;

@FunctionalInterface
public interface TreeBasedRefactoring extends Refactoring {
    @Override
    default String apply(Liblkqllang.AnalysisUnit unit) {
        var root = unit.getRoot();
        return (
            textRange(unit.getFirstToken(), root.tokenStart().previous()) +
            apply(root) +
            textRange(root.tokenEnd().next(), unit.getLastToken())
        );
    }

    String apply(Liblkqllang.LkqlNode root);
}
