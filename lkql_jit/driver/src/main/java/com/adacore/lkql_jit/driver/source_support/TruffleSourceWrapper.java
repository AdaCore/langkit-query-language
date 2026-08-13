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

/** A source defined from a Truffle source object. */
public final class TruffleSourceWrapper extends Source {

    // ----- Attributes -----

    /** The wrapped Truffle source. */
    private final com.oracle.truffle.api.source.Source truffleSource;

    // ----- Constructors -----

    TruffleSourceWrapper(com.oracle.truffle.api.source.Source truffleSource) {
        this.truffleSource = truffleSource;
    }

    // ----- Instance classes -----

    @Override
    public String getName() {
        return truffleSource.getName();
    }

    @Override
    public Optional<Path> getFile() {
        return Optional.ofNullable(truffleSource.getPath()).map(Paths::get);
    }

    @Override
    public List<String> getLines() {
        return getLines(0, truffleSource.getLineCount());
    }

    @Override
    public List<String> getLines(int from, int to) {
        var res = new ArrayList<String>(to - from);
        for (int i = from; i < to; i++) {
            res.add(truffleSource.getCharacters(i + 1).toString());
        }
        return res;
    }

    @Override
    public String getLineSeparator() {
        return getLineSeparator(truffleSource.getCharacters());
    }
}
