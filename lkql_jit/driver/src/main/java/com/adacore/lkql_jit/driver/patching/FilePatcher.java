//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.patching;

import com.github.difflib.DiffUtils;
import com.github.difflib.UnifiedDiffUtils;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.NavigableSet;
import java.util.TreeSet;

/**
 * Textual patcher for a single source file.
 *
 * <p>
 * This class holds the raw original content of the file (line separators are
 * preserved as-is), converts SARIF regions to character offsets, and applies
 * accepted edits by splicing the original content. All edits are expressed in
 * original file coordinates, so the file is written only once, after all the
 * fixes have been accepted, by applying the edits from the end of the file to
 * its beginning.
 */
public final class FilePatcher {

    /**
     * A textual edit expressed as a half-open range of character offsets in
     * the original file content. A zero-width range is a pure insertion.
     *
     * <p>
     * Edits are naturally ordered the way they appear in the patched content,
     * that is by offset, and, for a zero-width insertion sharing the offset of
     * a wider edit, the insertion first: text inserted at an offset comes
     * before the content which was at that offset.
     *
     * <p>
     * This is the single order all the operations on edits agree on: the
     * content is patched by applying the edits in its reverse, and the
     * original content is reconstructed by accounting for the length
     * variations in that same order. Without it, the result of a fix
     * inserting text at the very start of a range it also replaces would
     * depend on the order its edits happen to be listed in.
     */
    public record TextEdit(int startOffset, int endOffset, String replacement) implements
        Comparable<TextEdit> {
        /** Get whether this edit deletes nothing, and only inserts its text. */
        public boolean isInsertion() {
            return startOffset == endOffset;
        }

        /**
         * Get whether this edit and the given one cover a common part of the
         * content, which no pair of edits applied together may do.
         *
         * <p>
         * Two insertions need their own case: their ranges are empty, so they
         * never intersect, while inserting two texts at a same offset leaves
         * no way to tell which one comes first.
         */
        public boolean overlaps(TextEdit other) {
            if (isInsertion() && other.isInsertion()) {
                return startOffset == other.startOffset;
            }
            return (
                Math.max(startOffset, other.startOffset) < Math.min(endOffset, other.endOffset)
            );
        }

        /** Comparator implementing the order described above. */
        private static final Comparator<TextEdit> ORDER = Comparator.comparingInt(
            TextEdit::startOffset
        ).thenComparingInt(TextEdit::endOffset);

        @Override
        public int compareTo(TextEdit other) {
            return ORDER.compare(this, other);
        }
    }

    /** Line displayed in a diff for a content which has no final line separator. */
    private static final String NO_FINAL_SEPARATOR = "\\ No newline at end of file";

    /** Exception raised when a SARIF region does not fit in the target file. */
    public static final class InvalidRegionException extends Exception {

        public InvalidRegionException(String message) {
            super(message);
        }
    }

    // ----- Attributes -----

    /** The file patched by this instance. */
    private final Path file;

    /** Raw original content of the file. */
    private final String original;

    /** Charset which successfully decoded the file, used to write it back. */
    private final Charset charset;

    /** Offset of the first character of each 1-based line of the content. */
    private final int[] lineStartOffsets;

    /**
     * Edits applied by the previous runs, and edits accepted during this one,
     * all in original file coordinates. Both are kept sorted, and apart, since
     * the content the previous runs left is what this run starts from.
     *
     * <p>
     * Two edits covering the same range compare equal, so a set would hold
     * only one of them. This cannot lose an edit: such a pair overlaps, and
     * overlapping edits are rejected, by {@link #toEdits} within a fix and by
     * {@link #conflictsWithAccepted} between fixes, before reaching these.
     */
    private final NavigableSet<TextEdit> seeded = new TreeSet<>();

    private final NavigableSet<TextEdit> accepted = new TreeSet<>();

    /**
     * The URI the report uses to designate the file, so that the command names
     * it the way the report does. Note that this is not the URI of the file
     * itself, which "Path.toUri" would give: it is relative to a root the
     * report declares, which is what makes it short and independent from where
     * the sources happen to live.
     */
    public final String reportUri;

    // ----- Constructors -----

    private FilePatcher(
        Path file,
        String reportUri,
        String original,
        Charset charset,
        Collection<TextEdit> seeded
    ) {
        this.file = file;
        this.reportUri = reportUri;
        this.original = original;
        this.charset = charset;
        this.lineStartOffsets = computeLineStartOffsets(original);
        this.seeded.addAll(seeded);
    }

    /** Compute the offset of the first character of each line of the given content. */
    private static int[] computeLineStartOffsets(String content) {
        final var starts = new ArrayList<Integer>();
        starts.add(0);
        for (int i = 0; i < content.length(); i++) {
            if (content.charAt(i) == '\n') {
                starts.add(i + 1);
            }
        }
        return starts.stream().mapToInt(Integer::intValue).toArray();
    }

    /**
     * Read the given file, decoding it with the forced charset if any,
     * otherwise with UTF-8, falling back to ISO-8859-1 on decoding failure.
     */
    public static FilePatcher read(Path file, String reportUri, Charset forced) throws IOException {
        final var decoded = decodeWithFallback(Files.readAllBytes(file), forced);
        return new FilePatcher(file, reportUri, decoded.content(), decoded.charset(), List.of());
    }

    /**
     * Read the given file which was already patched by a previous run,
     * reconstructing its original content from the recorded history. The
     * edits of the history are seeded as already accepted, so that further
     * fixes are checked for conflicts against them. Return null when the
     * current content of the file does not match the recorded history, e.g.
     * because the file was modified externally.
     */
    public static FilePatcher readResuming(
        Path file,
        String reportUri,
        Charset forced,
        List<PatchState.AppliedEdit> history,
        String expectedOriginalHash
    ) throws IOException {
        final var decoded = decodeWithFallback(Files.readAllBytes(file), forced);
        final var original = reconstructOriginal(decoded.content(), history);
        if (original == null || !PatchState.hashOf(original).equals(expectedOriginalHash)) {
            return null;
        }
        final var seeded = history
            .stream()
            .map(edit -> new TextEdit(edit.startOffset(), edit.endOffset(), edit.insertedText()))
            .toList();
        return new FilePatcher(file, reportUri, original, decoded.charset(), seeded);
    }

    // ----- Instance methods -----

    /**
     * Convert the given region edits to offset-based textual edits, ensuring
     * that all regions fit in the file content and that edits of a same fix do
     * not overlap each other.
     */
    public List<TextEdit> toEdits(List<CandidateFix.RegionEdit> regionEdits)
        throws InvalidRegionException {
        final var edits = new ArrayList<TextEdit>();
        for (var regionEdit : regionEdits) {
            // A region ends after it starts, by construction, so converting
            // both of its positions gives a well ordered range of offsets
            final var start = offsetOf(regionEdit.startLine(), regionEdit.startColumn());
            final var end = offsetOf(regionEdit.endLine(), regionEdit.endColumn());

            // When the report tells which text is deleted, check that the
            // file still holds exactly it, line separators included: the
            // coordinates of a fix are only valid for the very sources the
            // report has been created from, and a file which differs by its
            // separators is not one of them
            if (
                regionEdit.isVerifiable() &&
                !original.substring(start, end).equals(regionEdit.expectedText())
            ) {
                throw new InvalidRegionException(
                    "deleted text at " +
                        regionEdit.startLine() +
                        ":" +
                        regionEdit.startColumn() +
                        " does not match the report, \"" +
                        reportUri +
                        "\" has changed since it was created"
                );
            }

            // Comparing the edit with the ones already converted is enough to
            // check every pair of the fix exactly once, and never an edit with
            // itself, which would always look like an overlap
            final var edit = new TextEdit(start, end, regionEdit.insertedText());
            for (var other : edits) {
                if (edit.overlaps(other)) {
                    throw new InvalidRegionException("fix contains overlapping replacements");
                }
            }
            edits.add(edit);
        }
        return edits;
    }

    /** Get whether any of the given edits overlaps an already applied edit. */
    public boolean conflictsWithAccepted(List<TextEdit> edits) {
        return edits.stream().anyMatch(edit -> allEdits().stream().anyMatch(edit::overlaps));
    }

    /** Accept the given edits, to be applied when the file is written. */
    public void accept(List<TextEdit> edits) {
        accepted.addAll(edits);
    }

    /** Get whether this patcher has edits accepted during this run. */
    public boolean hasNewEdits() {
        return !accepted.isEmpty();
    }

    /**
     * Get the SHA-256 digest of the bytes of the original content of the
     * file, to be compared with the digest a report declares for it. When
     * resuming a previous run, the original content is the reconstructed
     * pre-patching one, so that the digest of the report still matches.
     */
    public String originalBytesDigest() {
        return PatchState.hashOf(original.getBytes(charset));
    }

    /** Get the SHA-256 digest of the original content of the file. */
    public String originalHash() {
        return PatchState.hashOf(original);
    }

    /** Get all the accepted edits (seeded and new), with their deleted text. */
    public List<PatchState.AppliedEdit> acceptedHistory() {
        return allEdits()
            .stream()
            .map(edit ->
                new PatchState.AppliedEdit(
                    edit.startOffset(),
                    edit.endOffset(),
                    original.substring(edit.startOffset(), edit.endOffset()),
                    edit.replacement()
                )
            )
            .toList();
    }

    /** Get the unified diff previewing the application of the given edits alone. */
    public List<String> unifiedDiffFor(List<TextEdit> edits) {
        final var withFix = new TreeSet<>(seeded);
        withFix.addAll(edits);
        return unifiedDiff(applyToOriginal(withFix));
    }

    /** Get the unified diff of all the edits accepted during this run. */
    public List<String> cumulativeDiff() {
        return unifiedDiff(applyToOriginal(allEdits()));
    }

    /** Write the file with all accepted edits applied, using the original charset. */
    public void write() throws IOException {
        Files.write(file, applyToOriginal(allEdits()).getBytes(charset));
    }

    /**
     * Get whether the file still holds the content this patcher read from it,
     * that is whether writing it either did not happen or left it untouched.
     */
    public boolean isPristine() throws IOException {
        return Arrays.equals(Files.readAllBytes(file), contentWithSeeded().getBytes(charset));
    }

    /**
     * Write the file back as it was when this patcher read it, that is with
     * the edits of the previous runs, but without those accepted during this
     * one. Used to roll back a run which could not write all of its files.
     */
    public void restore() throws IOException {
        Files.write(file, contentWithSeeded().getBytes(charset));
    }

    // ----- Internal methods -----

    /** A decoded file content, with the charset which successfully decoded it. */
    private record Decoded(String content, Charset charset) {}

    /**
     * Decode the given bytes with the forced charset if any, otherwise with
     * UTF-8, falling back to ISO-8859-1 on decoding failure.
     */
    private static Decoded decodeWithFallback(byte[] bytes, Charset forced)
        throws CharacterCodingException {
        if (forced != null) {
            return new Decoded(decode(bytes, forced), forced);
        }
        try {
            return new Decoded(decode(bytes, StandardCharsets.UTF_8), StandardCharsets.UTF_8);
        } catch (CharacterCodingException e) {
            return new Decoded(
                decode(bytes, StandardCharsets.ISO_8859_1),
                StandardCharsets.ISO_8859_1
            );
        }
    }

    /** Decode the given bytes, raising a 'CharacterCodingException' on invalid input. */
    private static String decode(byte[] bytes, Charset charset) throws CharacterCodingException {
        return charset
            .newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .decode(ByteBuffer.wrap(bytes))
            .toString();
    }

    /**
     * Reconstruct the original content of a file from its current content
     * and the recorded history of applied edits (in original coordinates).
     * Return null when the current content does not match the history.
     */
    private static String reconstructOriginal(
        String current,
        List<PatchState.AppliedEdit> history
    ) {
        // Order the edits as they appear in the patched content, the same way
        // 'applyToOriginal' did when they were applied
        final var sorted = history.stream().sorted().toList();

        // Compute, for each edit, the offset shift introduced by the edits
        // located before it in the file.
        final var shifts = new int[sorted.size()];
        var shift = 0;
        for (int i = 0; i < sorted.size(); i++) {
            shifts[i] = shift;
            final var edit = sorted.get(i);
            shift += edit.insertedText().length() - (edit.endOffset() - edit.startOffset());
        }

        // Then replace, from the end of the file to its beginning, each
        // inserted text by the corresponding deleted text.
        final var result = new StringBuilder(current);
        for (int i = sorted.size() - 1; i >= 0; i--) {
            final var edit = sorted.get(i);
            final var start = edit.startOffset() + shifts[i];
            final var end = start + edit.insertedText().length();
            if (
                start < 0 ||
                end > result.length() ||
                !result.substring(start, end).equals(edit.insertedText())
            ) {
                return null;
            }
            result.replace(start, end, edit.deletedText());
        }
        return result.toString();
    }

    /** Get the content of the file as it was after the previously applied edits. */
    private String contentWithSeeded() {
        return applyToOriginal(seeded);
    }

    /**
     * Get the character offset of the given 1-based line and column. The
     * position just past the last line designates the end of the file, so
     * that a region may span up to "one line past the last, column 1".
     */
    private int offsetOf(int line, int column) throws InvalidRegionException {
        if (line >= 1 && line <= lineStartOffsets.length && column >= 1) {
            final var offset = lineStartOffsets[line - 1] + column - 1;
            if (offset <= contentEndOf(line)) {
                return offset;
            }
        } else if (line == lineStartOffsets.length + 1 && column == 1) {
            return original.length();
        }
        throw new InvalidRegionException(
            "position " + line + ":" + column + " is out of the bounds of \"" + reportUri + "\""
        );
    }

    /**
     * Get the offset just after the last character of the given 1-based line,
     * excluding its line separator. This is the last position a region may
     * designate on that line: the separator itself, and anything beyond it,
     * belongs to the following line, which a region must name explicitly.
     */
    private int contentEndOf(int line) {
        if (line == lineStartOffsets.length) {
            return original.length();
        }
        var end = lineStartOffsets[line] - 1;
        if (end > lineStartOffsets[line - 1] && original.charAt(end - 1) == '\r') {
            end--;
        }
        return end;
    }

    /** Get every edit applied to the file, by the previous runs and by this one. */
    private NavigableSet<TextEdit> allEdits() {
        final var result = new TreeSet<>(seeded);
        result.addAll(accepted);
        return result;
    }

    /** Apply the given edits on a copy of the original content, from the end to the beginning. */
    private String applyToOriginal(NavigableSet<TextEdit> edits) {
        final var result = new StringBuilder(original);
        for (var edit : edits.descendingSet()) {
            result.replace(edit.startOffset(), edit.endOffset(), edit.replacement());
        }
        return result.toString();
    }

    /**
     * Get the unified diff between the content the file currently holds, as
     * the previous runs left it, and the given patched content.
     */
    private List<String> unifiedDiff(String patched) {
        final var baseLines = splitForDisplay(contentWithSeeded());
        final var patch = DiffUtils.diff(baseLines, splitForDisplay(patched));
        final var name = reportUri;
        return UnifiedDiffUtils.generateUnifiedDiff("a/" + name, "b/" + name, baseLines, patch, 3);
    }

    /**
     * Split the given content in lines for diff display purposes, dropping
     * '\r'.
     *
     * <p>
     * The split keeps the trailing empty strings, so that lines added or
     * removed at the very end of the content appear in the diff. The last
     * one, produced by the final line separator, is not a line and is
     * dropped; when the content has no final separator, a marker line is
     * added instead, the way diff tools display that case. Without this, two
     * contents differing only by their trailing separators would produce an
     * empty diff, hiding from the user what a fix is about to change.
     */
    private static List<String> splitForDisplay(String content) {
        final var lines = new ArrayList<>(
            Arrays.stream(content.split("\n", -1))
                .map(line -> line.endsWith("\r") ? line.substring(0, line.length() - 1) : line)
                .toList()
        );
        if (!lines.isEmpty() && lines.getLast().isEmpty()) {
            lines.removeLast();
        } else if (!content.isEmpty()) {
            lines.add(NO_FINAL_SEPARATOR);
        }
        return lines;
    }
}
