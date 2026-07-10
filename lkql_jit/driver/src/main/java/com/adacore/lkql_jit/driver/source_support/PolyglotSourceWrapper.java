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

/** A source defined from a Polyglot source object. */
public final class PolyglotSourceWrapper extends Source {

    // ----- Attributes -----

    /** The wrapped polyglot source. */
    private final org.graalvm.polyglot.Source polyglotSource;

    // ----- Constructors -----

    PolyglotSourceWrapper(org.graalvm.polyglot.Source polyglotSource) {
        this.polyglotSource = polyglotSource;
    }

    // ----- Instance methods -----

    @Override
    public String getName() {
        return polyglotSource.getName();
    }

    @Override
    public Optional<Path> getFile() {
        return Optional.ofNullable(polyglotSource.getPath()).map(Paths::get);
    }

    @Override
    public List<String> getLines(int from, int to) {
        var res = new ArrayList<String>(to - from);
        for (int i = from; i < to; i++) {
            res.add(polyglotSource.getCharacters(i + 1).toString());
        }
        return res;
    }
}
