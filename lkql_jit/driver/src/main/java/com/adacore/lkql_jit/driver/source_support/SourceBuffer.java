//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/** A in-memory source that is defined by a name and a content. */
public final class SourceBuffer extends Source {

    // ----- Attributes -----

    /** Name of the source. */
    private final String name;

    /** Lines that defines the content of the source. */
    private final List<String> lines;

    // ----- Content -----

    SourceBuffer(String name, String content) {
        this.name = name;
        this.lines = SourceLinesCache.splitLines(content);
    }

    // ----- Instance methods -----

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Optional<Path> getFile() {
        return Optional.empty();
    }

    @Override
    public List<String> getLines(int from, int to) {
        return lines.subList(from, to);
    }
}
