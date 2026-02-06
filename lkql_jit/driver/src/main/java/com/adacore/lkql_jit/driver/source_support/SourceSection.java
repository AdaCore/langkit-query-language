//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import com.adacore.langkit_support.LangkitSupport;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/**
 * This interface materialize the abstract concept of "a source section" under a unified interface.
 */
public interface SourceSection {
    // ----- Constructors -----

    /** Wrap a Langkit token is a SourceSection object. */
    static SourceSection wrap(LangkitSupport.TokenInterface token, SourceLinesCache linesCache) {
        return new LangkitSlocRangeWrapper(
            token.getSourceLocationRange(),
            token.getUnit(),
            linesCache
        );
    }

    /** Wrap a Langkit node in a SourceSection object. */
    static SourceSection wrap(LangkitSupport.NodeInterface node, SourceLinesCache linesCache) {
        return new LangkitSlocRangeWrapper(
            node.getSourceLocationRange(),
            node.getUnit(),
            linesCache
        );
    }

    /** Wrap a Truffle source section in a SourceSection object. */
    static SourceSection wrap(com.oracle.truffle.api.source.SourceSection sourceSection) {
        return new TruffleSourceSectionWrapper(sourceSection);
    }

    // ----- Interface methods -----

    /** Get the name of the source. */
    String getSourceName();

    /** Return the path to the file this source section refers to, if applicable. */
    Optional<Path> getSourceFile();

    int startLine();

    /** Return the 1-indexed end line for this location */
    int endLine();

    /** Return the 1-indexed start column for this location */
    int startColumn();

    /** Return the 1-indexed end column for this location */
    int endColumn();

    /**
     * Return the lines of the source that this location spans, including the text outside the
     * strict span of this location, column-wise.
     */
    List<String> getLines();
}
