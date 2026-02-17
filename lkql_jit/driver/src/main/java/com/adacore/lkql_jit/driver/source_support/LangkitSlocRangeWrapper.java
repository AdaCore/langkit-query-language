//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import com.adacore.langkit_support.LangkitSupport;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

/** Wraps a Langkit's source location range into the SourceSection interface. */
public final class LangkitSlocRangeWrapper extends SourceSection {

    //  ----- Attributes ------

    public final LangkitSupport.SourceLocationRange wrappedSlocRange;

    /** The unit this source location range is located in. */
    public final LangkitSupport.AnalysisUnit unit;

    /** Cache to avoid loading source lines each time we access it. */
    private final SourceLinesCache linesCache;

    // ----- Constructors -----

    LangkitSlocRangeWrapper(
        LangkitSupport.SourceLocationRange slocRange,
        LangkitSupport.AnalysisUnit unit,
        SourceLinesCache linesCache
    ) {
        this.wrappedSlocRange = slocRange;
        this.unit = unit;
        this.linesCache = linesCache;
    }

    // ----- Instance methods -----

    @Override
    public String getSourceName() {
        return unit.getFileName(false);
    }

    @Override
    public Optional<Path> getSourceFile() {
        var filePath = Paths.get(unit.getFileName(true));
        return Files.isRegularFile(filePath) ? Optional.of(filePath) : Optional.empty();
    }

    @Override
    public int startLine() {
        return this.wrappedSlocRange.start.line;
    }

    @Override
    public int endLine() {
        return this.wrappedSlocRange.end.line;
    }

    @Override
    public int startColumn() {
        return this.wrappedSlocRange.start.column;
    }

    @Override
    public int endColumn() {
        return this.wrappedSlocRange.end.column - 1;
    }

    @Override
    public List<String> getLines() {
        return this.linesCache.getLines(unit).subList(startLine() - 1, endLine());
    }
}
