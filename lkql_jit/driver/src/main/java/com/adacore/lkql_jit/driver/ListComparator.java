//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver;

import java.util.Comparator;
import java.util.List;

/** This class defines a generic comparison for list of comparable elements. */
public final class ListComparator<T> implements Comparator<List<T>> {

    // ----- Attributes -----

    /** Comparator to compare list elements */
    private final Comparator<T> elemComparator;

    // ----- Constructors -----

    public ListComparator(Comparator<T> elemComparator) {
        this.elemComparator = elemComparator;
    }

    // ----- Instance methods -----

    @Override
    public int compare(List<T> l, List<T> r) {
        // Compare all elements of lists
        for (int i = 0; i < Math.max(l.size(), r.size()); i++) {
            // Ensure the index is in both lists
            if (i >= l.size()) return -1;
            else if (i >= r.size()) return 1;

            var elemCompare = elemComparator.compare(l.get(i), r.get(i));
            if (elemCompare != 0) return elemCompare;
        }

        // If we get here, lists are equals
        return 0;
    }
}
