//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.diagnostics.variants.BaseDiagnostic;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.diagnostics.variants.Info;
import com.adacore.lkql_jit.driver.diagnostics.variants.Warning;
import com.adacore.lkql_jit.driver.source_support.SourceLinesCache;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import com.adacore.lkql_jit.exceptions.LogLocation;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

/** Custom log handler to transform log elements into diagnostics. */
public final class DiagnosticLogHandler extends Handler {

    // ----- Attributes -----

    private final DiagnosticCollector diagnostics;

    private final SourceLinesCache linesCache;

    // ----- Constructors -----

    public DiagnosticLogHandler(DiagnosticCollector diagnostics, SourceLinesCache linesCache) {
        this.diagnostics = diagnostics;
        this.linesCache = linesCache;
    }

    // ----- Instance methods -----

    @Override
    public void publish(LogRecord record) {
        // Try to get the location of the log record
        SourceSection location = null;
        if (record.getThrown() != null) {
            switch (record.getThrown()) {
                case LogLocation e -> location = switch (e.location) {
                    case LogLocation.LangkitLocation l -> SourceSection.wrap(
                        l.locationRange,
                        l.unit,
                        linesCache
                    );
                    case LogLocation.TruffleLocation l -> SourceSection.wrap(l.sourceSection);
                };
                default -> {}
            }
        }

        // Create the new diagnostic object
        final BaseDiagnostic diagnostic;
        if (record.getLevel() == Level.SEVERE) {
            diagnostic = new Error(record.getMessage(), location);
        } else if (record.getLevel() == Level.WARNING) {
            diagnostic = new Warning(record.getMessage(), location);
        } else {
            diagnostic = new Info(record.getMessage(), location);
        }

        // Collect the diagnostic
        diagnostics.add(diagnostic);
    }

    @Override
    public void flush() {
        // Nothing happen because there is no output to flush
    }

    @Override
    public void close() {
        // Nothing happen because there is nothing to close
    }
}
