//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import com.adacore.langkit_support.LangkitSupport;
import com.oracle.truffle.api.nodes.Node;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/**
 * This interface materialize the abstract concept of "a source section" under a unified interface.
 */
public abstract class SourceSection {

    // ----- Constructors -----

    /** Wrap a Langkit token is a SourceSection object. */
    public static SourceSection wrap(
        LangkitSupport.TokenInterface token,
        SourceLinesCache linesCache
    ) {
        return new LangkitSlocRangeWrapper(
            token.getSourceLocationRange(),
            token.getUnit(),
            linesCache
        );
    }

    /** Wrap a Langkit node in a SourceSection object. */
    public static SourceSection wrap(
        LangkitSupport.NodeInterface node,
        SourceLinesCache linesCache
    ) {
        return new LangkitSlocRangeWrapper(
            node.getSourceLocationRange(),
            node.getUnit(),
            linesCache
        );
    }

    /** Wrap a Truffle source section in a SourceSection object. */
    public static SourceSection wrap(com.oracle.truffle.api.source.SourceSection sourceSection) {
        return new TruffleSourceSectionWrapper(sourceSection);
    }

    /** Wrap a Polyglot source section in a SourceSection object. */
    public static SourceSection wrap(org.graalvm.polyglot.SourceSection sourceSection) {
        return new PolyglotSourceSectionWrapper(sourceSection);
    }

    /**
     * Get the source section the provided node is contained in and wrap it into a SourceSection
     * object. This function may be recursive on node's parents, meaning that if the provided node
     * isn't related to Truffle source section, the function will recurse on its parent.
     */
    public static SourceSection wrap(Node node) {
        if (node.getSourceSection() == null) {
            var parent = node.getParent();
            return parent == null ? null : wrap(parent);
        }
        return wrap(node.getSourceSection());
    }

    // ----- Instance methods -----

    /** Get the name of the source. */
    public abstract String getSourceName();

    /** Return the path to the file this source section refers to, if applicable. */
    public abstract Optional<Path> getSourceFile();

    /** Return the 1-indexed start line for this location. */
    public abstract int startLine();

    /** Return the 1-indexed end line for this location. */
    public abstract int endLine();

    /** Return the 1-indexed start column for this location. */
    public abstract int startColumn();

    /** Return the 1-indexed end column for this location. */
    public abstract int endColumn();

    /**
     * Return the lines of the source that this location spans, including the text outside the
     * strict span of this location, column-wise.
     */
    public abstract List<String> getLines();

    /**
     * Get a string representation of this source section with the format
     * [source-name]:[start-line]:[start-column].
     */
    public String shortImage() {
        return getSourceName() + ":" + startLine() + ":" + startColumn();
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return (
            getSourceName() +
            '(' +
            startLine() +
            ':' +
            startColumn() +
            " - " +
            endLine() +
            ':' +
            endColumn() +
            ')'
        );
    }
}
