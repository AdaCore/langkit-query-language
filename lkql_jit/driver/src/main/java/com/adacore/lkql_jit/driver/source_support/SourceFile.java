//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/** A source that is defined by a file on the disk. */
public final class SourceFile extends Source {

    // ----- Attributes -----

    /** The file defining this source. */
    private final Path file;

    // ----- Constructor -----

    SourceFile(Path file) {
        this.file = file;
    }

    // ----- Instance methods -----

    @Override
    public String getName() {
        return file.getFileName().toString();
    }

    @Override
    public Optional<Path> getFile() {
        return Optional.of(file);
    }

    @Override
    public List<String> getLines(int from, int to) {
        return SOURCE_LINES_CACHE.getLines(file).subList(from, to);
    }
}
