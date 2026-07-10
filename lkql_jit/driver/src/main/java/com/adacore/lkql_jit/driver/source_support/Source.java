//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import com.adacore.langkit_support.LangkitSupport;
import com.oracle.truffle.api.CompilerDirectives;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.graalvm.collections.EconomicMap;

/** This abstract class materialize the concept of a "source" under a unified interface. */
public abstract sealed class Source
    permits SourceFile, SourceBuffer, TruffleSourceWrapper, PolyglotSourceWrapper {

    // ----- Attributes -----

    /** A cache used to fetch and cache lines of source files. */
    protected static SourceLinesCache SOURCE_LINES_CACHE = new SourceLinesCache();

    // ----- Constructors -----

    public static Source from(Path file) {
        return new SourceFile(file);
    }

    public static Source from(String name, String content) {
        return new SourceBuffer(name, content);
    }

    public static Source from(LangkitSupport.AnalysisUnit analysisUnit) {
        var unitFile = Paths.get(analysisUnit.getFileName(true));
        if (Files.isRegularFile(unitFile)) {
            return from(unitFile);
        } else {
            return from(analysisUnit.getFileName(false), analysisUnit.getText());
        }
    }

    public static Source from(com.oracle.truffle.api.source.Source truffleSource) {
        return new TruffleSourceWrapper(truffleSource);
    }

    public static Source from(org.graalvm.polyglot.Source polyglotSource) {
        return new PolyglotSourceWrapper(polyglotSource);
    }

    // ----- Instance methods -----

    /** Get the name of the source. */
    public abstract String getName();

    /** Return the path to the file this source section refers to, if applicable. */
    public abstract Optional<Path> getFile();

    /**
     * Get lines of the source in the provided bounds.
     *
     * @param from 0-based index of the included lower bound of the section to get.
     * @param to 0-based index of the excluded upper bound of the section.
     */
    public abstract List<String> getLines(int from, int to);

    // ----- Inner classes -----

    protected static class SourceLinesCache {

        // ----- Attributes -----

        private final EconomicMap<Path, List<String>> sourcesLines = EconomicMap.create();

        // ----- Instance methods -----

        /**
         * Return the lines of code composing the given file as a list of Strings. This either fetches
         * them from the cache if they were already computed previously, or computes them and stores
         * them in the cache for later reuse.
         *
         * @param sourcePath Path to the file to get the lines of.
         */
        @CompilerDirectives.TruffleBoundary
        protected List<String> getLines(Path sourcePath) {
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
        protected static List<String> splitLines(String text) {
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
}
