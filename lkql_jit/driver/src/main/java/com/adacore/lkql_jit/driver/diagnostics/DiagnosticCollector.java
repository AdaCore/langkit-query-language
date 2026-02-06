//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.diagnostics.variants.BaseDiagnostic;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Consumer;

/** A collector used to gather diagnostics during any LKQL execution. */
public final class DiagnosticCollector implements Iterable<BaseDiagnostic> {

    // ----- Attributes -----

    /** Collected diagnostics. */
    public final List<BaseDiagnostic> diagnostics;

    // ----- Constructors -----

    public DiagnosticCollector() {
        this.diagnostics = new ArrayList<>();
    }

    // ----- Instance methods -----

    public void add(BaseDiagnostic diagnostic) {
        this.diagnostics.add(diagnostic);
    }

    /** Get whether an error has been inserted in this diagnostic collector. */
    public boolean hasError() {
        return this.diagnostics.stream().anyMatch(d -> d instanceof Error);
    }

    /**
     * For each diagnostic in this collector, process it with the provided report creator.
     */
    public void createReport(Consumer<BaseDiagnostic> reportCreator) {
        for (var diagnostic : diagnostics) {
            reportCreator.accept(diagnostic);
        }
    }

    // ----- Override methods -----

    @Override
    public Iterator<BaseDiagnostic> iterator() {
        return this.diagnostics.iterator();
    }

    @Override
    public String toString() {
        return diagnostics.toString();
    }
}
