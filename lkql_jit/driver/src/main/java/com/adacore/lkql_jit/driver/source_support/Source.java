//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import com.adacore.langkit_support.LangkitSupport;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.Pair;

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

    /** Get all lines in this source. */
    public abstract List<String> getLines();

    /**
     * Get lines of the source in the provided bounds.
     *
     * @param from 0-based index of the included lower bound of the section to get.
     * @param to 0-based index of the excluded upper bound of the section.
     */
    public abstract List<String> getLines(int from, int to);

    /** Get the sequence that is separating lines in this source. */
    public abstract String getLineSeparator();

    // ----- Class methods -----

    /** Internal helper to get all lines from the provided string. */
    public static List<String> splitLines(String text) {
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

    /** Get the sequence in the provided text that is used to separate lines. */
    public static String getLineSeparator(CharSequence text) {
        for (int i = 0; i < text.length(); i++) {
            var c = text.charAt(i);
            if (c == '\n') {
                if (i > 0 && text.charAt(i - 1) == '\r') return "\r\n";
                else return "\n";
            }
        }

        // By default, return the system dependent line separator
        return System.lineSeparator();
    }

    // ----- Inner classes -----

    protected static class SourceLinesCache {

        // ----- Attributes -----

        /** A cache associating file paths to lines and a line separation sequence. */
        private final EconomicMap<Path, Pair<List<String>, String>> sourceCache =
            EconomicMap.create();

        // ----- Instance methods -----

        /**
         * Get the cache entry corresponding to the provided path, initializing it if there is none.
         */
        private Pair<List<String>, String> getCacheEntry(Path sourcePath) {
            var result = sourceCache.get(sourcePath, null);
            if (result == null) {
                try {
                    var fileContent = Files.readString(sourcePath);
                    result = Pair.create(
                        splitLines(fileContent),
                        Source.getLineSeparator(fileContent)
                    );
                    sourceCache.put(sourcePath, result);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            return result;
        }

        /**
         * Return the lines of code composing the given file as a list of Strings. This either
         * fetches them from the cache if they were already computed previously, or computes them
         * and stores them in the cache for later reuse.
         *
         * @param sourcePath Path to the file to get the lines of.
         */
        protected List<String> getLines(Path sourcePath) {
            return getCacheEntry(sourcePath).getLeft();
        }

        /**
         * Return the sequence used to separate lines in the file designated by the provided path.
         *
         * @param sourcePath Path to the file to get the lines of.
         */
        protected String getLineSeparator(Path sourcePath) {
            return getCacheEntry(sourcePath).getRight();
        }
    }
}
