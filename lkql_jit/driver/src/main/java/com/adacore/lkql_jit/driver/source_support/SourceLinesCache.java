//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import com.adacore.langkit_support.LangkitSupport;
import com.oracle.truffle.api.CompilerDirectives;
import java.util.ArrayList;
import java.util.List;
import org.graalvm.collections.EconomicMap;

/**
 * Caches source lines of analysis units to avoid recomputing them each time a diagnostic needs
 * to be emitted.
 */
public final class SourceLinesCache {

    // ----- Attributes -----

    private final EconomicMap<LangkitSupport.NodeInterface, List<String>> sourcesLines =
        EconomicMap.create();

    // ----- Instance methods -----

    /**
     * Return the lines of code composing the given analysis unit as an array of Strings. This
     * either fetches them from the cache if they were already computed previously, or computes
     * them and stores them in the cache for later reuse.
     *
     * @param unit The unit from which to extract source lines
     */
    @CompilerDirectives.TruffleBoundary
    public List<String> getLines(LangkitSupport.AnalysisUnit unit) {
        var root = unit.getRoot();
        var result = sourcesLines.get(root, null);
        if (result == null) {
            result = splitLines(unit.getText());
            sourcesLines.put(root, result);
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
