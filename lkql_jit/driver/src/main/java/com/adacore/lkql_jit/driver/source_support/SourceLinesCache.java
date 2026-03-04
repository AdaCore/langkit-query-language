//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import com.oracle.truffle.api.CompilerDirectives;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.graalvm.collections.EconomicMap;

/**
 * Caches source lines of analysis units to avoid recomputing them each time a diagnostic needs
 * to be emitted.
 */
public final class SourceLinesCache {

    // ----- Attributes -----

    private final EconomicMap<Path, List<String>> sourcesLines = EconomicMap.create();

    // ----- Instance methods -----

    /** Initialize lines associated to the provided path. */
    void initLines(Path sourcePath, String buffer) {
        sourcesLines.put(sourcePath, splitLines(buffer));
    }

    /**
     * Return the lines of code composing the given file as a list of Strings. This either fetches
     * them from the cache if they were already computed previously, or computes them and stores
     * them in the cache for later reuse.
     *
     * @param sourcePath Path to the file to get the lines of.
     */
    @CompilerDirectives.TruffleBoundary
    public List<String> getLines(Path sourcePath) {
        var result = sourcesLines.get(sourcePath, null);
        if (result == null) {
            try {
                result = splitLines(Files.readString(sourcePath));
                sourcesLines.put(sourcePath, result);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        return result;
    }

    // ----- Class methods -----

    /** Internal helper to get all lines from the provided string. */
    private static List<String> splitLines(String text) {
        List<String> res = new ArrayList<>();
        StringBuilder buffer = new StringBuilder();
        for (int i = 0; i < text.length(); i++) {
            var c = text.charAt(i);
            if (c == '\n') {
                res.add(buffer.toString());
                buffer.setLength(0);
            } else if (c != '\r') {
                buffer.append(c);
            }
        }
        res.add(buffer.toString());
        return res;
    }
}
