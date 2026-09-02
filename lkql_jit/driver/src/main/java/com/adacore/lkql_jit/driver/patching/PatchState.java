//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.patching;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Persistent record of the fixes already applied from a given SARIF report,
 * stored as a JSON file next to the report.
 *
 * <p>
 * The state is keyed by the hash of the report: a state produced from
 * another version of the report is discarded, since a regenerated report
 * expresses its fixes against the current content of the files. For each
 * patched file, the state records the hash of its original content, the
 * fingerprints of the applied fixes, and the applied edits (with the deleted
 * text), so that a later run can reconstruct the original content, verify
 * it, and apply the remaining fixes at their exact original coordinates.
 *
 * <p>
 * Recording the deleted and inserted texts means this file holds fragments
 * of the patched sources, including text a fix has removed from them, in
 * clear. It is created next to the report, with the default permissions, and
 * is not covered by any ignore rule: protecting it, and keeping it out of
 * commits and of build artifacts, is up to whoever runs the command. It only
 * holds the memory of the fixes already applied, so deleting it is always
 * safe: the next run simply considers that no fix has been applied yet.
 */
public final class PatchState {

    /** Exception raised when an existing state cannot be read. */
    public static final class InvalidStateException extends Exception {

        public InvalidStateException(String message) {
            super(message);
        }
    }

    /**
     * An edit applied during a previous run, in original file coordinates.
     * Edits are naturally ordered the way they appear in the patched content,
     * as {@link FilePatcher.TextEdit} are, so that reconstructing a content
     * accounts for their length variations in the order they were applied in.
     */
    public record AppliedEdit(
        int startOffset,
        int endOffset,
        String deletedText,
        String insertedText
    ) implements Comparable<AppliedEdit> {
        /** Comparator implementing the order described above. */
        private static final Comparator<AppliedEdit> ORDER = Comparator.comparingInt(
            AppliedEdit::startOffset
        ).thenComparingInt(AppliedEdit::endOffset);

        @Override
        public int compareTo(AppliedEdit other) {
            return ORDER.compare(this, other);
        }
    }

    /** The patch state of a single file. */
    public record FileState(
        String originalHash,
        List<AppliedEdit> edits,
        Set<String> appliedFixes
    ) {}

    // ----- Attributes -----

    /** Hash of the SARIF report this state was produced from. */
    private final String reportHash;

    /** Per-file states, keyed by the image of an absolute normalized path. */
    private final Map<String, FileState> files;

    // ----- Constructors -----

    private PatchState(String reportHash, Map<String, FileState> files) {
        this.reportHash = reportHash;
        this.files = files;
    }

    /** Create an empty state for the given report hash. */
    public static PatchState empty(String reportHash) {
        return new PatchState(reportHash, new HashMap<>());
    }

    /**
     * Load the state stored in the given file. An empty state is returned
     * when the file is missing, unreadable, or was produced from another
     * version of the report.
     */
    public static PatchState load(Path stateFile, String reportHash) throws InvalidStateException {
        if (!Files.exists(stateFile)) {
            return empty(reportHash);
        }
        try {
            final var json = new JSONObject(Files.readString(stateFile, StandardCharsets.UTF_8));
            if (!reportHash.equals(json.optString("report_hash"))) {
                return empty(reportHash);
            }
            final var files = new HashMap<String, FileState>();
            final var jsonFiles = json.getJSONObject("files");
            for (var path : jsonFiles.keySet()) {
                final var jsonFile = jsonFiles.getJSONObject(path);
                final var edits = new ArrayList<AppliedEdit>();
                for (var jsonEdit : jsonFile.getJSONArray("edits")) {
                    final var edit = (JSONObject) jsonEdit;
                    edits.add(
                        new AppliedEdit(
                            edit.getInt("start"),
                            edit.getInt("end"),
                            edit.getString("deleted"),
                            edit.getString("inserted")
                        )
                    );
                }
                final var appliedFixes = new HashSet<String>();
                for (var fingerprint : jsonFile.getJSONArray("applied_fixes")) {
                    appliedFixes.add((String) fingerprint);
                }
                // Normalize the key read from the file, as the one of a lookup
                // is: a state written by another version of the command, or
                // edited by hand, must still be found
                files.put(
                    keyOf(Path.of(path)),
                    new FileState(jsonFile.getString("original_hash"), edits, appliedFixes)
                );
            }
            return new PatchState(reportHash, files);
        } catch (IOException | JSONException | ClassCastException | InvalidPathException e) {
            // The state exists but cannot be read: it is unknown which fixes
            // have already been applied, so the caller must not proceed as if
            // none had been
            throw new InvalidStateException(e.getMessage());
        }
    }

    // ----- Instance methods -----

    /**
     * Get the key under which the state of the given file is stored: the image
     * of its absolute normalized path, so that all the spellings of a same path
     * designate a single state.
     *
     * <p>
     * Normalizing is purely lexical, and never touches the file system: the
     * state of a file which has been removed, or which has never existed, must
     * still be reachable, if only to be discarded. Recognizing two paths
     * reaching a same file through different symbolic links is therefore up to
     * the caller, which is expected to have canonicalized them already.
     */
    private static String keyOf(Path file) {
        return file.toAbsolutePath().normalize().toString();
    }

    /** Get the state of the given file, or null if there is none. */
    public FileState fileState(Path file) {
        return files.get(keyOf(file));
    }

    /** Set the state of the given file. */
    public void setFileState(Path file, FileState state) {
        files.put(keyOf(file), state);
    }

    /** Get whether the given fix is recorded as applied in all its target files. */
    public boolean isApplied(CandidateFix fix) {
        final var fingerprint = fix.fingerprint();
        return fix
            .changes()
            .stream()
            .allMatch(change -> {
                final var state = fileState(change.file());
                return state != null && state.appliedFixes().contains(fingerprint);
            });
    }

    /** Write this state to the given file. */
    public void save(Path stateFile) throws IOException {
        final var jsonFiles = new JSONObject();
        for (var entry : files.entrySet()) {
            final var state = entry.getValue();
            final var jsonEdits = new JSONArray();
            for (var edit : state.edits()) {
                jsonEdits.put(
                    new JSONObject()
                        .put("start", edit.startOffset())
                        .put("end", edit.endOffset())
                        .put("deleted", edit.deletedText())
                        .put("inserted", edit.insertedText())
                );
            }
            jsonFiles.put(
                entry.getKey(),
                new JSONObject()
                    .put("original_hash", state.originalHash())
                    .put("applied_fixes", new JSONArray(state.appliedFixes()))
                    .put("edits", jsonEdits)
            );
        }
        final var json = new JSONObject()
            .put("version", 1)
            .put("report_hash", reportHash)
            .put("files", jsonFiles);

        // Write through a temporary file, then move it in place: an
        // interrupted write must not leave a half written state behind, as it
        // could then no longer be told which fixes have been applied
        final var temporary = stateFile.resolveSibling(stateFile.getFileName() + ".tmp");
        try {
            Files.writeString(temporary, json.toString(4) + "\n", StandardCharsets.UTF_8);
            try {
                Files.move(
                    temporary,
                    stateFile,
                    StandardCopyOption.ATOMIC_MOVE,
                    StandardCopyOption.REPLACE_EXISTING
                );
            } catch (AtomicMoveNotSupportedException e) {
                Files.move(temporary, stateFile, StandardCopyOption.REPLACE_EXISTING);
            }
        } catch (IOException e) {
            Files.deleteIfExists(temporary);
            throw e;
        }
    }

    // ----- Hashing -----

    /** Get the SHA-256 hex digest of the given bytes. */
    public static String hashOf(byte[] bytes) {
        final MessageDigest digest;
        try {
            digest = MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException e) {
            throw new AssertionError("SHA-256 is always available", e);
        }
        return HexFormat.of().formatHex(digest.digest(bytes));
    }

    /** Get the SHA-256 hex digest of the given text, encoded as UTF-8. */
    public static String hashOf(String text) {
        return hashOf(text.getBytes(StandardCharsets.UTF_8));
    }
}
