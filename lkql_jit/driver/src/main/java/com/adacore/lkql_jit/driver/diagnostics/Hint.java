//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.Comparator;

/**
 * Represents a hint for a diagnostic. A hint is a located additional information you want to attach
 * to a diagnostic.
 */
public record Hint(String message, SourceSection location) implements Comparable<Hint> {
    @Override
    public int compareTo(Hint other) {
        return Comparator.comparing(Hint::location)
            .thenComparing(Hint::message)
            .compare(this, other);
    }

    @Override
    public boolean equals(Object o) {
        if (o == null) return false;
        if (!(o instanceof Hint other)) return false;
        return compareTo(other) == 0;
    }
}
