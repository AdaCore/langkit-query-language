//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.diagnostics.variants.BaseDiagnostic;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.exceptions.LKQLStaticErrors;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.TruffleStackTraceElement;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.StreamSupport;
import org.graalvm.polyglot.PolyglotException;

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

    /**
     * Helper function that provide support to handle an exception from the execution of a source
     * in a polyglot context. This function tries to extract an LKQL specific exception from the
     * origin one and dispatch its handling accordingly.
     * Provided hints are associated to all created diagnostics.
     */
    public void handleException(PolyglotException exception, Hint... hints) {
        // Check if the exception is from an LKQL execution
        if (exception.isGuestException()) {
            var guestException = exception.getGuestObject();
            if (guestException != null) {
                var truffleException = exception
                    .getGuestObject()
                    .as(AbstractTruffleException.class);
                switch (truffleException) {
                    case LKQLEngineException e:
                        handleException(e, hints);
                        return;
                    case LKQLRuntimeError e:
                        handleException(e, hints);
                        return;
                    case LKQLStaticErrors e:
                        handleException(e, hints);
                        return;
                    default:
                }
            }
        }

        // Handle the polyglot exception the generic way
        var location = exception.getSourceLocation();
        var diagnostic = new Error(
            exception.getMessage(),
            location == null ? null : SourceSection.wrap(location)
        );
        Arrays.stream(hints).forEach(diagnostic::addHint);

        // Try to get the LKQL call stack
        var lkqlStackTrace = StreamSupport.stream(
            exception.getPolyglotStackTrace().spliterator(),
            false
        )
            .filter(PolyglotException.StackFrame::isGuestFrame)
            .toList();
        if (!lkqlStackTrace.isEmpty()) {
            for (var frame : lkqlStackTrace) {
                diagnostic.addCallToStack(
                    frame.getRootName(),
                    SourceSection.wrap(frame.getSourceLocation())
                );
            }
        }

        // Finally add the diagnostic to the collector
        add(diagnostic);
    }

    /** Handle an LKQL engine exception and fill the diagnostic collector accordingly. */
    public void handleException(LKQLEngineException exception, Hint... hints) {
        var location = exception.getEncapsulatingSourceSection();
        var diagnostic = new Error(
            exception.getMessage(),
            location == null ? null : SourceSection.wrap(location)
        );
        Arrays.stream(hints).forEach(diagnostic::addHint);
        add(diagnostic);
    }

    /** Handle an LKQL runtime error and fill the diagnostic collector accordingly. */
    public void handleException(LKQLRuntimeError error, Hint... hints) {
        // Create the base error diagnostic
        var errorNode = error.getLocation();
        var diagnostic = new Error(
            error.getMessage(),
            errorNode == null ? null : SourceSection.wrap(errorNode)
        );
        Arrays.stream(hints).forEach(diagnostic::addHint);

        // Get the LKQL stack trace
        List<TruffleStackTraceElement> stackTrace = TruffleStackTrace.getStackTrace(error);

        // Skip the first frame because it is the error location
        stackTrace = stackTrace.isEmpty() ? stackTrace : stackTrace.subList(1, stackTrace.size());

        // Then fill the error diagnostic with the call stack
        for (var frame : stackTrace) {
            var frameNode = frame.getLocation();
            if (frameNode != null) {
                var callLocation = SourceSection.wrap(frameNode);
                var callContext = frame.getTarget().getRootNode().getName();
                if (callLocation != null) {
                    diagnostic.addCallToStack(callContext, callLocation);
                }
            }
        }

        // Finally add the diagnostic to the collector
        add(diagnostic);
    }

    /** Handle LKQL static errors and fill the diagnostic collector accordingly. */
    public void handleException(LKQLStaticErrors errors, Hint... hints) {
        for (var staticError : errors.diagnostics) {
            var diagnostic = new Error(
                staticError.message(),
                SourceSection.wrap(staticError.location())
            );
            Arrays.stream(hints).forEach(diagnostic::addHint);
            add(diagnostic);
        }
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

    /** Clear this diagnostic collector by deleting all collected diagnostics. */
    public void clear() {
        diagnostics.clear();
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
