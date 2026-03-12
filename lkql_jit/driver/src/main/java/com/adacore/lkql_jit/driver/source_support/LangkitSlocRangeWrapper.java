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

    /** Full file path of the wrapped analysis unit. */
    private final Path unitFile;

    /** Cache to avoid loading source lines each time we access it. */
    private final SourceLinesCache linesCache;

    // ----- Constructors -----

    LangkitSlocRangeWrapper(
        LangkitSupport.SourceLocationRange slocRange,
        LangkitSupport.AnalysisUnit unit,
        SourceLinesCache linesCache
    ) {
        this.wrappedSlocRange = slocRange;
        this.unitFile = Paths.get(unit.getFileName(true));
        this.linesCache = linesCache;

        // Fetch lines of the unit if it is not from a file
        if (!Files.isRegularFile(unitFile)) {
            linesCache.initLines(unitFile, unit.getText());
        }
    }

    // ----- Instance methods -----

    @Override
    public String getSourceName() {
        return unitFile.getFileName().toString();
    }

    @Override
    public Optional<Path> getSourceFile() {
        return Files.isRegularFile(unitFile) ? Optional.of(unitFile) : Optional.empty();
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
        return this.wrappedSlocRange.end.column;
    }

    @Override
    public List<String> getLines() {
        return linesCache.getLines(unitFile).subList(startLine() - 1, endLine());
    }
}
