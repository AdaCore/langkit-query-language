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

    /** Wrap a Langkit token is a SourceSection object. */
    public static SourceSection wrap(LangkitSupport.TokenInterface token) {
        return wrap(token.getSourceLocationRange(), token.getUnit());
    }

    /** Wrap a Langkit node in a SourceSection object. */
    public static SourceSection wrap(LangkitSupport.NodeInterface node) {
        return wrap(node.getSourceLocationRange(), node.getUnit());
    }

    /** Wrap a langkit source location with its related analysis unit in a SourceSection object. */
    public static SourceSection wrap(
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

    /** Wrap a Truffle source section in a SourceSection object. */
    public static SourceSection wrap(com.oracle.truffle.api.source.SourceSection sourceSection) {
        return new SourceSection(
            Source.from(sourceSection.getSource()),
            sourceSection.getStartLine(),
            sourceSection.getStartColumn(),
            sourceSection.getEndLine(),
            sourceSection.getEndColumn() + 1
        );
    }

    /** Wrap a Polyglot source section in a SourceSection object. */
    public static SourceSection wrap(org.graalvm.polyglot.SourceSection sourceSection) {
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
    public static SourceSection wrap(Node node) {
        if (node.getSourceSection() == null) {
            var parent = node.getParent();
            return parent == null ? null : wrap(parent);
        }
        return wrap(node.getSourceSection());
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
