//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.source_support;

import com.adacore.langkit_support.LangkitSupport;
import com.oracle.truffle.api.nodes.Node;
import java.util.List;

/** Represents a section in a source. */
public record SourceSection(
    Source source,
    int startLine,
    int startColumn,
    int endLine,
    int endColumn
) {
    // ----- Constructors -----

    /** Create a new source section from a Langkit token. */
    public static SourceSection from(LangkitSupport.TokenInterface token) {
        return from(token.getSourceLocationRange(), token.getUnit());
    }

    /** Create a new source section from a Langkit node. */
    public static SourceSection from(LangkitSupport.NodeInterface node) {
        return from(node.getSourceLocationRange(), node.getUnit());
    }

    /** Create a new source section from a Langkit location range and a related analysis unit. */
    public static SourceSection from(
        LangkitSupport.SourceLocationRange locationRange,
        LangkitSupport.AnalysisUnit unit
    ) {
        return new SourceSection(
            Source.from(unit),
            locationRange.start.line,
            locationRange.start.column,
            locationRange.end.line,
            locationRange.end.column
        );
    }

    /** Create a new source section from a Truffle one. */
    public static SourceSection from(com.oracle.truffle.api.source.SourceSection sourceSection) {
        return new SourceSection(
            Source.from(sourceSection.getSource()),
            sourceSection.getStartLine(),
            sourceSection.getStartColumn(),
            sourceSection.getEndLine(),
            sourceSection.getEndColumn() + 1
        );
    }

    /** Create a new source section from a Polyglot one. */
    public static SourceSection from(org.graalvm.polyglot.SourceSection sourceSection) {
        return new SourceSection(
            Source.from(sourceSection.getSource()),
            sourceSection.getStartLine(),
            sourceSection.getStartColumn(),
            sourceSection.getEndLine(),
            sourceSection.getEndColumn() + 1
        );
    }

    /**
     * Get the source section the provided node is contained in and wrap it into a SourceSection
     * object. This function may be recursive on node's parents, meaning that if the provided node
     * isn't related to Truffle source section, the function will recurse on its parent.
     */
    public static SourceSection from(Node node) {
        if (node.getSourceSection() == null) {
            var parent = node.getParent();
            return parent == null ? null : from(parent);
        }
        return from(node.getSourceSection());
    }

    // ----- Instance methods -----

    /**
     * Get a string representation of this source section with the format
     * [source-name]:[start-line]:[start-column].
     */
    public String shortImage() {
        return source().getName() + ":" + startLine() + ":" + startColumn();
    }

    /**
     * Return the lines of the source that this location spans, including the text outside the
     * strict span of this location, column-wise.
     */
    public List<String> getLines() {
        return source.getLines(startLine - 1, endLine);
    }
}
