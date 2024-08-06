//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.source_location;

import com.adacore.liblkqllang.Liblkqllang;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;

/** Implementation of SourceLocation wrapping Truffle's SourceSections. */
public class SourceSectionWrapper implements SourceLocation {

    public static SourceSection createSection(Liblkqllang.SourceLocationRange sloc, Source source) {
        int end_col = sloc.end.column - 1;
        if (sloc.start.line == sloc.end.line) {
            end_col = Math.max(sloc.end.column - 1, sloc.start.column);
        }
        return source.createSection(sloc.start.line, sloc.start.column, sloc.end.line, end_col);
    }

    public static SourceSectionWrapper create(Liblkqllang.SourceLocationRange sloc, Source source) {
        return new SourceSectionWrapper(createSection(sloc, source));
    }

    public final SourceSection sourceSection;

    public SourceSectionWrapper(SourceSection section) {
        this.sourceSection = section;
    }

    @Override
    public int startLine() {
        return this.sourceSection.getStartLine();
    }

    @Override
    public int endLine() {
        return this.sourceSection.getEndLine();
    }

    @Override
    public int startColumn() {
        return this.sourceSection.getStartColumn();
    }

    @Override
    public int endColumn() {
        return this.sourceSection.getEndColumn();
    }

    @Override
    public String[] getLines() {
        var ret = new ArrayList<String>();
        for (int i = this.startLine(); i <= this.endLine(); i++) {
            ret.add(this.sourceSection.getSource().getCharacters(i).toString());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String fileName() {
        if (sourceSection.getSource().getPath() != null) {
            return Paths.get(sourceSection.getSource().getPath()).getFileName().toString();
        }
        return sourceSection.getSource().getName();
    }

    @Override
    public File getDir() {
        if (sourceSection.getSource().getPath() != null) {
            return new File(sourceSection.getSource().getPath()).getParentFile();
        }
        return null;
    }

    @Override
    public String toString() {
        return "<SourceSection " + display() + ":" + endLine() + ":" + endColumn() + ">";
    }
}
