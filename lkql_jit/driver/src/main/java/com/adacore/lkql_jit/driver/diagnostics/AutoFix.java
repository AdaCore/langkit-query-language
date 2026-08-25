//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.Styling;
import com.adacore.lkql_jit.driver.source_support.Source;
import com.github.difflib.DiffUtils;
import com.github.difflib.patch.Patch;
import com.github.difflib.text.DiffRow;
import com.github.difflib.text.DiffRowGenerator;
import de.jcup.sarif_2_1_0.model.*;
import java.net.URI;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Result of an LKQL auto-fix function application.
 *
 * @param targetSource Source to apply the patch to.
 * @param patch The patch computed from the auto-fix function.
 */
public record AutoFix(Source targetSource, Patch<String> patch) {
    // ----- Attributes -----

    /** Generator used to turn patches into diff rows. */
    static DiffRowGenerator diffRowGenerator = DiffRowGenerator.create()
        .lineNormalizer(s -> s)
        .build();

    // ----- Instance methods -----

    /**
     * Get a pretty and easy to read representation of this auto-fix result.
     *
     * @param linePrefix Add this string at the beginning of each line of the pretty auto fix
     *                   representation.
     * @param margin Number of lines to display before and after each change.
     * @param withStyle Whether the style the pretty representation with ANSI codes.
     */
    public String toPrettyString(String linePrefix, int margin, boolean withStyle) {
        // Define styling functions
        Styling.StylingFunction lineNumStyle = withStyle ? Styling::blue : s -> s;
        Styling.StylingFunction ellipsisStyle = withStyle ? Styling::brightBlack : s -> s;
        Styling.StylingFunction addStyle = withStyle ? Styling::green : s -> s;
        Styling.StylingFunction deleteStyle = withStyle ? Styling::red : s -> s;

        // Create the result
        var res = new ArrayList<List<String>>();

        // Get lines to display as batches
        var filteredBatches = toBatches(getFilteredDiffLines(margin), false);

        // Compute the maximum line number
        var lastLine = filteredBatches.getLast().getLast().lineNum;
        var lastLineWidth = Integer.toString(lastLine).length();

        // Create the blank prefix
        var blankPrefix = linePrefix + lineNumStyle.apply(" ".repeat(lastLineWidth)) + ' ';

        // Now add batches to the result
        for (var batch : filteredBatches) {
            var batchBuffer = new ArrayList<String>();
            var insertBuffer = new ArrayList<String>();

            for (var diffLine : batch) {
                var diffRow = diffLine.diff;
                var lineNum = diffLine.lineNum;

                // Create line prefix
                var prefix =
                    linePrefix +
                    lineNumStyle.apply(
                        lineNum + " ".repeat(lastLineWidth - Integer.toString(lineNum).length())
                    ) +
                    ' ';

                // Now dispatch according to the diff row tag to display it
                switch (diffRow.getTag()) {
                    case EQUAL -> {
                        batchBuffer.addAll(insertBuffer);
                        insertBuffer.clear();
                        batchBuffer.add(prefix + ' ' + diffRow.getNewLine());
                    }
                    case INSERT -> insertBuffer.add(
                        blankPrefix + addStyle.apply('+' + diffRow.getNewLine())
                    );
                    case DELETE -> batchBuffer.add(
                        prefix + deleteStyle.apply('-' + diffRow.getOldLine())
                    );
                    case CHANGE -> {
                        insertBuffer.add(blankPrefix + addStyle.apply('+' + diffRow.getNewLine()));
                        batchBuffer.add(prefix + deleteStyle.apply('-' + diffRow.getOldLine()));
                    }
                }
            }

            // Add the insertion buffer to the batch buffer if it isn't empty
            if (!insertBuffer.isEmpty()) batchBuffer.addAll(insertBuffer);

            // Then add the batch buffer to the result
            res.add(batchBuffer);
        }

        // Then return the result as a multiline string
        return String.join(
            "\n" + blankPrefix + ' ' + ellipsisStyle.apply("...") + "\n",
            res
                .stream()
                .map(b -> String.join("\n", b))
                .toList()
        );
    }

    /** Create a new SARIF artifact change object representing this auto-fix and return it. */
    public ArtifactChange toArtifactChange() {
        // Create the result object
        var res = new ArtifactChange();

        // Get line separator used by the fixes source
        var lineSep = targetSource.getLineSeparator();

        // Get all diff batches
        var diffBatches = toBatches(getFilteredDiffLines(0), true);

        // Set the target file of the change
        var changedArtifact = new ArtifactLocation();
        changedArtifact.setUri(
            targetSource
                .getFile()
                .map(Path::toUri)
                .map(URI::toString)
                .orElse(targetSource.getName())
        );
        res.setArtifactLocation(changedArtifact);

        // Process all batches to create SARIF replacement objects
        var replacements = new ArrayList<Replacement>(diffBatches.size());
        for (var batch : diffBatches) {
            // If the batch is a collection of changes, handle each separately to create precise
            // replacement object inside changed lines.
            if (batch.getFirst().diff.getTag() == DiffRow.Tag.CHANGE) {
                for (var diffLine : batch) {
                    var inlinePatch = diffLine.getInlineDiff();
                    for (var delta : inlinePatch.getDeltas()) {
                        // Create the replacement object
                        var replacement = new Replacement();

                        // Get the column number where the delta start in the source
                        var startCol = delta.getSource().getPosition() + 1;

                        // Get the text replaced in the source
                        var replacedText = delta.getSource().getLines().stream().findFirst();

                        // Create the object describing the region that is being deleted
                        var deletedRegion = new Region();
                        deletedRegion.setStartLine(diffLine.lineNum);
                        deletedRegion.setStartColumn(startCol);
                        deletedRegion.setEndLine(diffLine.lineNum);
                        deletedRegion.setEndColumn(
                            startCol + replacedText.map(String::length).orElse(0)
                        );

                        // Set the code snippet that is being deleted
                        replacedText.ifPresent(t -> {
                            var snippet = new ArtifactContent();
                            snippet.setText(t);
                            deletedRegion.setSnippet(snippet);
                        });

                        // Finally, place the deleted region in the replacement object
                        replacement.setDeletedRegion(deletedRegion);

                        // Now create the content to insert
                        delta
                            .getTarget()
                            .getLines()
                            .stream()
                            .findFirst()
                            .ifPresent(s -> {
                                var insertedContent = new ArtifactContent();
                                insertedContent.setText(s);
                                replacement.setInsertedContent(insertedContent);
                            });

                        // Finally, add the replacement object to the result
                        replacements.add(replacement);
                    }
                }
            }
            // Otherwise, the batch is insertions or deletions
            else {
                // Create the resulting replacement object
                var replacement = new Replacement();

                // Create the object representing the region to remove
                var deletedRegion = new Region();
                deletedRegion.setStartLine(batch.getFirst().lineNum);
                deletedRegion.setStartColumn(1);
                deletedRegion.setEndColumn(1);
                replacement.setDeletedRegion(deletedRegion);

                if (batch.getFirst().diff.getTag() == DiffRow.Tag.INSERT) {
                    // Set the deleted region length to 1 to express an insertion
                    deletedRegion.setEndLine(batch.getFirst().lineNum);

                    // Then, create the inserted content
                    var insertedContent = new ArtifactContent();
                    insertedContent.setText(
                        batch
                            .stream()
                            .map(l -> l.diff.getNewLine() + lineSep)
                            .collect(Collectors.joining())
                    );
                    replacement.setInsertedContent(insertedContent);
                } else {
                    // If the diff is a deletion, set the end of the deleted region to the start of
                    // the line next to the batch.
                    deletedRegion.setEndLine(batch.getLast().lineNum + 1);

                    // Then set the snippet of the deleted region to the text of the removed batch
                    var snippet = new ArtifactContent();
                    snippet.setText(
                        batch
                            .stream()
                            .map(l -> l.diff.getOldLine() + lineSep)
                            .collect(Collectors.joining())
                    );
                    deletedRegion.setSnippet(snippet);
                }

                // Finally, add the replacement object to the result
                replacements.add(replacement);
            }
        }

        // Set the replacements of the result and return it
        res.setReplacements(replacements);
        return res;
    }

    /** Get all lines of the target source associated to their diff. */
    private List<DiffLine> getDiffLines() {
        var res = new ArrayList<DiffLine>();
        var currentLineNum = 1;
        for (var diffRow : diffRowGenerator.generateDiffRows(targetSource.getLines(), patch)) {
            res.add(new DiffLine(currentLineNum, diffRow));
            if (diffRow.getTag() != DiffRow.Tag.INSERT) currentLineNum++;
        }
        return res;
    }

    /**
     * Get all lines with a change in this auto-fix associated to their 1-based number in the file.
     *
     * @param margin Add this count of lines with no change around lines with one.
     */
    private List<DiffLine> getFilteredDiffLines(int margin) {
        var res = new ArrayList<DiffLine>();
        var diffRowsWithLineNum = getDiffLines();

        // Filter out all useless lines
        for (int i = 0; i < diffRowsWithLineNum.size(); i++) {
            // Get the current line
            var diffLine = diffRowsWithLineNum.get(i);

            // Get whether it should appear in the final result
            var addRow = diffLine.diff.getTag() != DiffRow.Tag.EQUAL;
            if (!addRow) {
                for (int offset = 1; offset <= margin; offset++) {
                    addRow |= ((i - offset >= 0 &&
                            diffRowsWithLineNum.get(i - offset).diff.getTag() !=
                            DiffRow.Tag.EQUAL) ||
                        (i + offset < diffRowsWithLineNum.size() &&
                            diffRowsWithLineNum.get(i + offset).diff.getTag() !=
                            DiffRow.Tag.EQUAL));
                }
            }

            // If required the line to the result
            if (addRow) res.add(diffLine);
        }

        // Finally, return the result
        return res;
    }

    /** Group diff lines into contiguous groups of diffs. */
    private static List<List<DiffLine>> toBatches(List<DiffLine> diffLines, boolean splitByTag) {
        var res = new ArrayList<List<DiffLine>>();
        var currentBatch = new ArrayList<DiffLine>();

        // For each diff line, if it is contiguous to the previous one add it to the buffer
        for (var diffLine : diffLines) {
            if (
                !currentBatch.isEmpty() &&
                (diffLine.lineNum > currentBatch.getLast().lineNum + 1 ||
                    (splitByTag && diffLine.diff.getTag() != currentBatch.getLast().diff.getTag()))
            ) {
                res.add(currentBatch);
                currentBatch = new ArrayList<>();
            }
            currentBatch.add(diffLine);
        }

        // Add the last batch if there is one
        if (!currentBatch.isEmpty()) {
            res.add(currentBatch);
        }

        // Finally, return the result
        return res;
    }

    // ----- Inner classes -----

    /** Store a diff applied to the line designated by the associated number. */
    private record DiffLine(int lineNum, DiffRow diff) {
        private Patch<String> getInlineDiff() {
            return DiffUtils.diffInline(diff.getOldLine(), diff.getNewLine());
        }
    }
}
