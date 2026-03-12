//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/** This class wraps Truffle's source section in the SourceSection interface. */
public final class TruffleSourceSectionWrapper extends SourceSection {

    // ----- Attribubtes -----

    public final com.oracle.truffle.api.source.SourceSection wrappedSourceSection;

    // ----- Constructors -----

    TruffleSourceSectionWrapper(com.oracle.truffle.api.source.SourceSection sourceSection) {
        this.wrappedSourceSection = sourceSection;
    }

    // ----- Instance methods -----

    @Override
    public String getSourceName() {
        return this.wrappedSourceSection.getSource().getName();
    }

    @Override
    public Optional<Path> getSourceFile() {
        var sourcePath = this.wrappedSourceSection.getSource().getPath();
        if (sourcePath != null) {
            return Optional.of(Paths.get(sourcePath));
        } else {
            return Optional.empty();
        }
    }

    @Override
    public int startLine() {
        return this.wrappedSourceSection.getStartLine();
    }

    @Override
    public int endLine() {
        return this.wrappedSourceSection.getEndLine();
    }

    @Override
    public int startColumn() {
        return this.wrappedSourceSection.getStartColumn();
    }

    @Override
    public int endColumn() {
        return this.wrappedSourceSection.getEndColumn() + 1;
    }

    @Override
    public List<String> getLines() {
        var ret = new ArrayList<String>();
        for (int i = this.startLine(); i <= this.endLine(); i++) {
            ret.add(this.wrappedSourceSection.getSource().getCharacters(i).toString());
        }
        return ret;
    }
}
