//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.patching;

import java.nio.file.Path;
import java.util.List;

/**
 * A quick fix extracted from a SARIF report, candidate for application on the
 * source files it targets.
 *
 * <p>
 * Textual edits are expressed in SARIF region coordinates: 1-based lines and
 * columns, columns are character-based and end-exclusive. A zero-width region
 * represents a pure insertion, and an edit without inserted text represents a
 * pure deletion.
 */
public record CandidateFix(
    String ruleId,
    String message,
    String locationImage,
    List<FileChange> changes
) {
    /**
     * Get a stable fingerprint identifying this fix: its rule and the exact
     * edits it performs. It is what the record of the applied fixes holds, so
     * that a fix is recognized across the runs on a same report.
     */
    public String fingerprint() {
        final var image = new StringBuilder(ruleId);
        for (var change : changes) {
            image.append('\0').append(change.file());
            for (var edit : change.edits()) {
                image
                    .append('\0')
                    .append(edit.startLine())
                    .append(':')
                    .append(edit.startColumn())
                    .append('-')
                    .append(edit.endLine())
                    .append(':')
                    .append(edit.endColumn())
                    .append('\0')
                    .append(edit.insertedText());
            }
        }
        return PatchState.hashOf(image.toString());
    }

    /**
     * All the edits of a fix targeting a single file. The URI is the one the
     * report states, kept to designate the file the way the report does.
     */
    public record FileChange(Path file, String uri, List<RegionEdit> edits) {}

    /**
     * A single textual edit: the region to delete, in original file
     * coordinates, and the text to insert in place of it (the empty string
     * for a pure deletion).
     *
     * <p>
     * When the report provides a snippet for the deleted region,
     * 'expectedText' holds it and the patcher checks that the file really
     * contains that text at those coordinates before editing it. It is null
     * when the report provides no snippet, in which case the edit is applied
     * at its recorded coordinates without any verification.
     */
    public record RegionEdit(
        int startLine,
        int startColumn,
        int endLine,
        int endColumn,
        String insertedText,
        String expectedText
    ) {
        public RegionEdit {
            // A region ending before it starts denotes no text at all, which
            // no file can make sense of
            if (endLine < startLine || (endLine == startLine && endColumn < startColumn)) {
                throw new IllegalArgumentException(
                    "region ends at " +
                        endLine +
                        ":" +
                        endColumn +
                        ", before its start at " +
                        startLine +
                        ":" +
                        startColumn
                );
            }
        }

        /** Get whether the report tells which text this edit is deleting. */
        public boolean isVerifiable() {
            return expectedText != null && !expectedText.isEmpty();
        }
    }
}
