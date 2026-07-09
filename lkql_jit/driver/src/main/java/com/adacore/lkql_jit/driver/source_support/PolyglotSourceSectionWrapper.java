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

/** This class wraps Polyglot source section in the SourceSection interface. */
public final class PolyglotSourceSectionWrapper extends SourceSection {

    // ----- Attribubtes -----

    public final org.graalvm.polyglot.SourceSection wrappedSourceSection;

    // ----- Constructors -----

    PolyglotSourceSectionWrapper(org.graalvm.polyglot.SourceSection sourceSection) {
        this.wrappedSourceSection = sourceSection;
    }

    // ----- Instance methods -----

    @Override
    public String getSourceName() {
        return this.wrappedSourceSection.getSource().getName();
    }

    @Override
    public Optional<Path> getSourceFile() {
        return Optional.ofNullable(this.wrappedSourceSection.getSource().getPath()).map(Paths::get);
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
