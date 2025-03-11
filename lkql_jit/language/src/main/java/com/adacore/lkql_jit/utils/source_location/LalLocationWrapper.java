//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.source_location;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.utils.functions.FileUtils;
import java.io.File;

/**
 * LAL implementation of SourceLocation.
 *
 * <p>TODO: At some point we want to use the generic baseclass in the shared langkit Java library.
 * See eng/libadalang/langkit-query-language#269
 */
public class LalLocationWrapper implements SourceLocation {

    public final Libadalang.SourceLocationRange sourceLocationRange;
    public final CheckerUtils.SourceLinesCache sourceLinesCache;
    private final LangkitSupport.AnalysisUnit unit;

    public LalLocationWrapper(
        Libadalang.SourceLocationRange sourceLocationRange,
        LangkitSupport.AnalysisUnit unit,
        CheckerUtils.SourceLinesCache sourceLinesCache
    ) {
        this.sourceLocationRange = sourceLocationRange;
        this.unit = unit;
        this.sourceLinesCache = sourceLinesCache;
    }

    public LalLocationWrapper(
        LangkitSupport.NodeInterface node,
        CheckerUtils.SourceLinesCache sourceLinesCache
    ) {
        this.sourceLocationRange = node.getSourceLocationRange();
        this.unit = node.getUnit();
        this.sourceLinesCache = sourceLinesCache;
    }

    @Override
    public int startLine() {
        return sourceLocationRange.start.line;
    }

    @Override
    public int endLine() {
        return sourceLocationRange.end.line;
    }

    @Override
    public int startColumn() {
        return sourceLocationRange.start.column;
    }

    @Override
    public int endColumn() {
        return sourceLocationRange.end.column - 1;
    }

    @Override
    public String[] getLines() {
        var sourceLines = this.sourceLinesCache.getLines(unit);
        String[] validLines = new String[(endLine() + 1) - startLine()];
        if (endLine() + 1 - startLine() >= 0) {
            System.arraycopy(
                sourceLines,
                startLine() - 1,
                validLines,
                0,
                endLine() + 1 - startLine()
            );
        }
        return validLines;
    }

    @Override
    public String fileName() {
        return FileUtils.baseName(unit.getFileName());
    }

    @Override
    public File getDir() {
        return new File(unit.getFileName()).getParentFile();
    }

    @Override
    public String toString() {
        return ("<LALLocation " + display() + ":" + endLine() + ":" + endColumn() + ">");
    }
}
