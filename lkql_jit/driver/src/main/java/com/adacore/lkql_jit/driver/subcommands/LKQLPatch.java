//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.lkql_jit.driver.Styling;
import com.adacore.lkql_jit.driver.diagnostics.TextReportCreator;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.diagnostics.variants.Warning;
import com.adacore.lkql_jit.driver.patching.CandidateFix;
import com.adacore.lkql_jit.driver.patching.FilePatcher;
import com.adacore.lkql_jit.driver.patching.PatchState;
import com.adacore.lkql_jit.driver.patching.SarifFixLoader;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.AccessDeniedException;
import java.nio.file.FileSystemException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.graalvm.options.OptionCategory;
import org.graalvm.polyglot.Context.Builder;
import picocli.CommandLine;

/**
 * Patch command for LKQL. Applies the quick fixes contained in a SARIF report
 * (as produced by the LKQL checker) to the source files they target.
 *
 * <p>
 * Fixes are read from the "results[].fixes[]" objects of the report, with the
 * usual SARIF conventions: regions are 1-based, character-based and
 * end-exclusive, a zero-width region denotes a pure insertion, and a
 * replacement without inserted content denotes a pure deletion. All the
 * replacements of a fix are expressed in original file coordinates, so files
 * are written once, at the end of the run, and fixes overlapping an already
 * accepted fix are rejected as conflicts.
 *
 * <p>
 * By default the command is interactive: each fix is displayed with a unified
 * diff preview and the user answers 'y' (apply), 'n' (skip), 'a' (apply this
 * fix and all the remaining ones of the same rule), 'A' (apply this fix and
 * all the remaining ones, whatever their rule), 'q' (quit, skipping the
 * remaining fixes) or 'h' (display what each answer means). The '--auto' mode
 * applies all fixes without prompting.
 *
 * <p>
 * The files to patch are the ones the report designates: a relative URI is
 * resolved from the root its "uriBaseId" names among the ones the report
 * declares in "originalUriBaseIds", while an absolute URI needs no root and is
 * taken as it is. Every path is canonicalized, with symbolic links followed, so
 * that a same file is always recognized as such.
 *
 * <p>
 * Applied fixes are recorded in a state file, next to the report (see
 * {@link PatchState}), which makes reruns idempotent: a fix is never applied
 * twice, and a run stopped with 'q' can be resumed later by simply rerunning
 * the command. Deleting the state file resets this memory. The state is
 * invalidated automatically when the report is regenerated or when a patched
 * file is modified externally. Note that it records the text each fix has
 * removed and inserted, so it holds source fragments in clear, and it is up
 * to the user to protect it accordingly.
 */
@CommandLine.Command(
    name = "patch",
    description = "Apply quick fixes from a SARIF report to source files",
    mixinStandardHelpOptions = true
)
public class LKQLPatch extends BaseSubcommand {

    // ----- CLI arguments -----

    @CommandLine.Parameters(
        index = "0",
        paramLabel = "SARIF_FILE",
        description = "SARIF report containing the fixes to apply"
    )
    public Path sarifFile;

    @CommandLine.Parameters(
        index = "1..*",
        paramLabel = "SOURCES",
        description = "If provided, only apply fixes targeting these sources. Each of them may" +
            " be a directory, selecting all the files under it, a file, or a bare file name," +
            " selecting the files bearing it wherever they are"
    )
    public List<String> sources = new ArrayList<>();

    @CommandLine.Option(
        names = { "--exclude-rule" },
        paramLabel = "<rule>",
        split = ",",
        description = "Do not apply the fixes coming from the given rules. May be repeated," +
            " and accepts a comma separated list"
    )
    public List<String> excludedRules = new ArrayList<>();

    @CommandLine.Option(
        names = { "--list-rules" },
        description = "Display the rules of the report, with the number of fixes each of them" +
            " provides, then exit without modifying anything"
    )
    public boolean listRules;

    @CommandLine.Option(
        names = { "-a", "--auto" },
        description = "Apply all fixes without prompting"
    )
    public boolean auto;

    @CommandLine.Option(
        names = { "--dry-run" },
        description = "Go through the fixes but do not modify any file"
    )
    public boolean dryRun;

    @CommandLine.Option(
        names = { "-C", "--charset" },
        description = "Charset of the source files (default: utf-8, with iso-8859-1 fallback)"
    )
    public String charset;

    // ----- Execution state -----

    /** Patchers of the files targeted by the fixes, created on first use. */
    private final Map<Path, FilePatcher> patchers = new LinkedHashMap<>();

    /** Files which cannot be patched, with the reason why, to report it only once. */
    private final Map<Path, String> brokenFiles = new HashMap<>();

    /** File recording the fixes applied from the report. */
    private Path stateFile;

    /** Charset forced by the '--charset' option, if any. */
    private Charset forcedCharset = null;

    /** Rules whose remaining fixes are applied without prompting, after an "a" answer. */
    private final Set<String> autoRules = new HashSet<>();

    /** Canonical files, directories and bare file names given as SOURCES. */
    private final Set<Path> sourceFiles = new HashSet<>();

    private final List<Path> sourceDirectories = new ArrayList<>();
    private final Set<String> sourceNames = new HashSet<>();

    /** SHA-256 digests the report declares for the files it analyzed. */
    private Map<Path, String> digests = Map.of();

    /** Files whose content has been checked against the digest of the report. */
    private final Set<Path> verifiedFiles = new HashSet<>();

    /** State of the fixes applied by previous runs on the same report. */
    private PatchState state;

    /** Whether the state changed during this run and must be saved. */
    private boolean stateDirty = false;

    /** Whether the user asked, when quitting, to keep no record of this run. */
    private boolean discardState = false;

    /** Fingerprints of the fixes applied during this run, per target file. */
    private final Map<Path, Set<String>> newlyApplied = new LinkedHashMap<>();

    /** Counters for the final summary. */
    private int appliedCount = 0;

    private int skippedCount = 0;
    private int conflictCount = 0;
    private int errorCount = 0;
    private int alreadyAppliedCount = 0;
    private int unverifiedCount = 0;

    // ----- Command execution -----

    @Override
    public Integer call() {
        final var returnCode = patch();
        diagnostics.createReport(new TextReportCreator(System.err, supportAnsi));
        return returnCode;
    }

    /** Run the whole patching process and return the command exit code. */
    private Integer patch() {
        // Validate the CLI arguments
        if (charset != null) {
            try {
                forcedCharset = Charset.forName(charset);
            } catch (Exception e) {
                diagnostics.add(new Error("Unknown charset \"" + charset + "\""));
                return 1;
            }
        }
        if (!Files.isRegularFile(sarifFile)) {
            diagnostics.add(new Error("Cannot read the SARIF report \"" + sarifFile + "\""));
            return 1;
        }
        // Load the fixes from the SARIF report
        final var maybeFixes = SarifFixLoader.load(sarifFile.toAbsolutePath(), diagnostics);
        if (maybeFixes.isEmpty()) {
            return 1;
        }

        // Only display the rules of the report when asked to
        if (listRules) {
            printRules(maybeFixes.get().fixesPerRule());
            return diagnostics.hasError() ? 1 : 0;
        }
        digests = maybeFixes.get().digests();

        // Load the state left by previous runs on the same report, if any
        stateFile = Path.of(sarifFile.toAbsolutePath() + ".applied");
        try {
            state = PatchState.load(stateFile, PatchState.hashOf(Files.readAllBytes(sarifFile)));
        } catch (IOException e) {
            diagnostics.add(new Error("Cannot read the SARIF report \"" + sarifFile + "\""));
            return 1;
        } catch (PatchState.InvalidStateException e) {
            diagnostics.add(
                new Error(
                    "Cannot read the patch state \"" +
                        stateFile +
                        "\": " +
                        e.getMessage() +
                        ". It records the fixes already applied from this report: remove it to" +
                        " start over, at the risk of applying some of them twice."
                )
            );
            return 1;
        }

        // Leave out the fixes of the excluded rules, then filter the
        // remaining ones according to the provided sources
        final var excluded = excludedRules
            .stream()
            .map(rule -> rule.toLowerCase())
            .collect(Collectors.toSet());
        prepareSourceFilters();
        final var eligible = new ArrayList<CandidateFix>();
        var filteredCount = 0;
        var excludedCount = 0;
        for (var fix : maybeFixes.get().fixes()) {
            if (excluded.contains(fix.ruleId().toLowerCase())) {
                excludedCount++;
            } else if (!matchesSources(fix)) {
                filteredCount++;
            } else {
                eligible.add(fix);
            }
        }
        if (eligible.isEmpty()) {
            final var reasons = new ArrayList<String>();
            if (excludedCount > 0) {
                reasons.add(excludedCount + " coming from excluded rules");
            }
            if (filteredCount > 0) {
                reasons.add(filteredCount + " filtered out by the source filters");
            }
            System.out.println(
                "No applicable fix in \"" +
                    sarifFile +
                    "\"" +
                    (reasons.isEmpty() ? "" : " (" + String.join(", ", reasons) + ")") +
                    "."
            );
            return diagnostics.hasError() ? 1 : 0;
        }

        // In automatic mode no one reviews the diffs, so warn upfront when the
        // report gives no way to check the sources are the ones it was created
        // from: fixes are then applied at their recorded coordinates, whatever
        // the sources contain there now
        if (auto && eligible.stream().noneMatch(this::isVerifiable)) {
            System.out.println(
                styled(
                    "Warning: this report provides neither digest nor deleted text snippet," +
                        " fixes will be applied without checking the sources have not changed" +
                        " since it was created.",
                    Styling::yellow
                )
            );
            System.out.println();
        }

        // Process each fix, prompting the user unless in automatic mode
        final var input = new BufferedReader(new InputStreamReader(System.in));
        var quit = false;
        for (int i = 0; i < eligible.size() && !quit; i++) {
            final var fix = eligible.get(i);
            System.out.println(
                styled(
                        "[" + (i + 1) + "/" + eligible.size() + "] " + fix.ruleId() + ": ",
                        Styling::bold
                    ) +
                    fix.message()
            );
            System.out.println(styled("  at " + fix.locationImage(), Styling::brightBlue));

            // Convert the fix regions to textual edits on the target files
            final var perFileEdits = editsOf(fix);
            if (perFileEdits == null) {
                errorCount++;
                System.out.println();
                continue;
            }

            // Skip the fix if a previous run on this report already applied it
            if (state.isApplied(fix)) {
                System.out.println(styled("-> already applied", Styling::brightBlue));
                System.out.println();
                alreadyAppliedCount++;
                continue;
            }

            // Reject the fix if it overlaps an already accepted fix
            if (
                perFileEdits
                    .entrySet()
                    .stream()
                    .anyMatch(e -> patchers.get(e.getKey()).conflictsWithAccepted(e.getValue()))
            ) {
                System.out.println(
                    styled("-> skipped (conflicts with a previously accepted fix)", Styling::red)
                );
                System.out.println();
                conflictCount++;
                continue;
            }

            // Ask the user what to do with the fix, unless every fix, or
            // every fix of its rule, has already been accepted in advance
            var answer = "y";
            if (!auto && !autoRules.contains(fix.ruleId())) {
                perFileEdits.forEach((file, edits) ->
                    printDiff(patchers.get(file).unifiedDiffFor(edits))
                );
                answer = ask(input, fix.ruleId());
            }
            switch (answer) {
                case "y", "a", "A" -> {
                    final var fingerprint = fix.fingerprint();
                    perFileEdits.forEach((file, edits) -> {
                        patchers.get(file).accept(edits);
                        newlyApplied.computeIfAbsent(file, f -> new HashSet<>()).add(fingerprint);
                    });
                    appliedCount++;
                    if (!isVerified(fix)) {
                        unverifiedCount++;
                    }
                    switch (answer) {
                        case "a" -> {
                            autoRules.add(fix.ruleId());
                            System.out.println(
                                styled(
                                    "-> applying all remaining fixes of \"" + fix.ruleId() + "\"",
                                    Styling::green
                                )
                            );
                        }
                        case "A" -> {
                            auto = true;
                            System.out.println(
                                styled("-> applying all remaining fixes", Styling::green)
                            );
                        }
                        default -> System.out.println(styled("-> applied", Styling::green));
                    }
                }
                case "n" -> {
                    skippedCount++;
                    System.out.println(styled("-> skipped", Styling::yellow));
                }
                default -> {
                    quit = true;
                    skippedCount += eligible.size() - i;
                    System.out.println(styled("-> quit", Styling::yellow));

                    // Leaving the run in the middle is where keeping, or not,
                    // the record of what has been applied really matters, so
                    // it is the moment to ask. There is nothing to decide when
                    // no record exists and none would be written, and nothing
                    // can be asked when the input is exhausted.
                    if (
                        answer.equals("q") &&
                        !dryRun &&
                        (appliedCount > 0 || Files.exists(stateFile))
                    ) {
                        discardState = !askKeepState(input);
                    }
                }
            }
            System.out.println();
        }

        // Collect the files to modify
        final var toWrite = new ArrayList<FilePatcher>();
        for (var patcher : patchers.values()) {
            if (!patcher.hasNewEdits()) {
                continue;
            }
            toWrite.add(patcher);
        }

        // In dry-run mode, only display the cumulative diff of each file
        if (dryRun) {
            for (var patcher : toWrite) {
                printDiff(patcher.cumulativeDiff());
                System.out.println();
            }
        }

        // Otherwise write all the files, then record the applied fixes. The
        // fixes of a run are applied as a whole: if any file cannot be
        // written, the files already written are restored and no fix is
        // recorded, so that the whole run can be retried once the cause of
        // the failure has been fixed. Recording a partially written run would
        // make its fixes unapplicable forever, as the recorded edits of the
        // written files would conflict with the fixes to retry.
        var modifiedFilesCount = 0;
        if (!dryRun) {
            final var written = new ArrayList<FilePatcher>();
            var failedWrite = false;
            for (var patcher : toWrite) {
                // The file is added to the ones to restore before being
                // written: writing truncates it first, so a failure in the
                // middle leaves it incomplete, and it has to be restored just
                // like those which were fully written
                written.add(patcher);
                try {
                    patcher.write();
                } catch (IOException e) {
                    diagnostics.add(
                        new Error("Cannot write \"" + patcher.reportUri + "\": " + reasonOf(e))
                    );
                    failedWrite = true;
                    break;
                }
            }

            if (failedWrite) {
                rollback(written, "a file could not be written");
            } else if (discardState) {
                forgetState();
                modifiedFilesCount = written.size();
            } else if (!recordAppliedFixes() && !written.isEmpty()) {
                // Which fixes have been applied could not be recorded, so a
                // later run would apply them again: undo them all
                rollback(written, "the patch state could not be written");
            } else {
                modifiedFilesCount = written.size();
            }
        } else {
            modifiedFilesCount = toWrite.size();
        }

        // Print the final summary
        System.out.println(
            styled(
                (modifiedFilesCount > 0
                            ? (dryRun ? "Would patch " : "Patched ") +
                              modifiedFilesCount +
                              " file(s): "
                            : "") +
                    appliedCount +
                    " fix(es) applied, " +
                    (alreadyAppliedCount > 0 ? alreadyAppliedCount + " already applied, " : "") +
                    skippedCount +
                    " skipped, " +
                    conflictCount +
                    " conflict(s), " +
                    errorCount +
                    " error(s).",
                Styling::bold
            )
        );
        if (excludedCount > 0) {
            System.out.println(excludedCount + " fix(es) coming from excluded rules.");
        }
        if (filteredCount > 0) {
            System.out.println(filteredCount + " fix(es) filtered out by the source filters.");
        }
        if (unverifiedCount > 0) {
            System.out.println(
                unverifiedCount + " fix(es) applied without verifying the targeted sources."
            );
        }
        if (dryRun) {
            System.out.println("Dry run: no file was modified.");
        }
        return diagnostics.hasError() ? 1 : 0;
    }

    // ----- Internal methods -----

    /**
     * Display the rules of the report with the number of fixes each of them
     * provides, so that they can be passed to the '--exclude-rule' option.
     */
    private void printRules(Map<String, Integer> fixesPerRule) {
        if (fixesPerRule.isEmpty()) {
            System.out.println("No rule in \"" + sarifFile + "\".");
            return;
        }
        final var width = fixesPerRule.keySet().stream().mapToInt(String::length).max().orElse(0);
        System.out.println(
            styled(
                "\"" + sarifFile + "\" contains " + fixesPerRule.size() + " rule(s):",
                Styling::bold
            )
        );
        fixesPerRule.forEach((rule, count) ->
            System.out.println(
                "  " + String.format("%-" + width + "s", rule) + "  " + count + " fix(es)"
            )
        );
    }

    /**
     * Restore the given files as they were before this run, after a write
     * failure has interrupted it. The whole run is cancelled, so nothing is
     * recorded and everything can be retried.
     */
    private void rollback(List<FilePatcher> written, String reason) {
        appliedCount = 0;
        unverifiedCount = 0;
        var allRestored = true;
        for (var patcher : written) {
            try {
                // A file whose writing failed before it even started still
                // holds its original content, and rewriting it would fail
                // again for the very same reason
                if (patcher.isPristine()) {
                    continue;
                }
                patcher.restore();
            } catch (IOException e) {
                allRestored = false;
                diagnostics.add(
                    new Error(
                        "Cannot restore \"" +
                            patcher.reportUri +
                            "\" after a failed run, it may contain partially applied fixes: " +
                            e.getMessage()
                    )
                );
            }
        }
        System.out.println(
            styled(
                allRestored
                    ? "All the fixes of this run have been cancelled and no source has been" +
                      " modified: " +
                      reason +
                      "."
                    : "All the fixes of this run have been cancelled, but some sources could" +
                      " not be restored: " +
                      reason +
                      ".",
                Styling::red
            )
        );
    }

    /**
     * Get a message describing the given I/O failure. The message of the
     * common file system exceptions is just the name of the file, which is
     * of no use in a diagnostic already naming it.
     */
    private static String reasonOf(IOException e) {
        return switch (e) {
            case NoSuchFileException _ -> "no such file";
            case AccessDeniedException _ -> "permission denied";
            case FileSystemException f -> f.getReason() == null
                ? "file system error"
                : f.getReason();
            default -> e.getMessage();
        };
    }

    /**
     * Record the fixes applied during this run in the state file, and return
     * whether the state is up to date. Failing to record them means a later
     * run cannot know they have been applied, so the caller has to undo them.
     */
    private boolean recordAppliedFixes() {
        for (var entry : patchers.entrySet()) {
            final var patcher = entry.getValue();
            if (!patcher.hasNewEdits()) {
                continue;
            }
            final var previous = state.fileState(entry.getKey());
            final var fingerprints = new HashSet<String>(
                previous == null ? Set.of() : previous.appliedFixes()
            );
            fingerprints.addAll(newlyApplied.getOrDefault(entry.getKey(), Set.of()));
            state.setFileState(
                entry.getKey(),
                new PatchState.FileState(
                    patcher.originalHash(),
                    patcher.acceptedHistory(),
                    fingerprints
                )
            );
            stateDirty = true;
        }
        if (stateDirty) {
            try {
                state.save(stateFile);
            } catch (IOException e) {
                diagnostics.add(
                    new Error("Cannot write the patch state \"" + stateFile + "\": " + reasonOf(e))
                );
                return false;
            }
        }
        return true;
    }

    /**
     * Get whether the report gives a way to check the application of the
     * given fix, either through a digest of its target files or through the
     * text its edits delete.
     */
    private boolean isVerifiable(CandidateFix fix) {
        if (
            fix
                .changes()
                .stream()
                .allMatch(change -> digests.containsKey(change.file()))
        ) {
            return true;
        }
        return fix
            .changes()
            .stream()
            .flatMap(change -> change.edits().stream())
            .allMatch(CandidateFix.RegionEdit::isVerifiable);
    }

    /**
     * Get whether the application of the given fix has been verified: either
     * all the files it targets have been checked against a digest of the
     * report, or all its edits told which text they delete, and that text has
     * been found. Note that a pure insertion deletes nothing, so it can only
     * be verified through a digest.
     */
    private boolean isVerified(CandidateFix fix) {
        if (
            fix
                .changes()
                .stream()
                .allMatch(change -> verifiedFiles.contains(change.file()))
        ) {
            return true;
        }
        return fix
            .changes()
            .stream()
            .flatMap(change -> change.edits().stream())
            .allMatch(CandidateFix.RegionEdit::isVerifiable);
    }

    /**
     * Resolve the SOURCES arguments once, sorting them in the three ways they
     * may designate the files to patch: a directory selects everything under
     * it, a path selects the file it designates, and a bare file name selects
     * the files bearing it, wherever they are.
     */
    private void prepareSourceFilters() {
        for (var source : sources) {
            final var path = Path.of(source);
            final var canonical = SarifFixLoader.canonical(path);
            if (Files.isDirectory(canonical)) {
                sourceDirectories.add(canonical);
            } else {
                sourceFiles.add(canonical);
                if (path.getNameCount() == 1) {
                    sourceNames.add(source);
                }
            }
        }
    }

    /** Get whether the given fix passes the SOURCES filter. */
    private boolean matchesSources(CandidateFix fix) {
        if (sources.isEmpty()) {
            return true;
        }
        for (var change : fix.changes()) {
            final var file = change.file();
            if (
                !sourceFiles.contains(file) &&
                !sourceNames.contains(file.getFileName().toString()) &&
                sourceDirectories.stream().noneMatch(file::startsWith)
            ) {
                return false;
            }
        }
        return true;
    }

    /**
     * Get the offset-based edits of the given fix, grouped by target file.
     * Return null when the fix cannot be applied, after having reported the
     * reason and printed a skipping notice.
     */
    private Map<Path, List<FilePatcher.TextEdit>> editsOf(CandidateFix fix) {
        // Group the edits of the fix by target file: a fix may contain
        // several changes for a same file, and all the edits it performs on
        // it must be converted, and checked for overlap, together
        final var editsByFile = new LinkedHashMap<Path, List<CandidateFix.RegionEdit>>();
        final var uriByFile = new LinkedHashMap<Path, String>();
        for (var change : fix.changes()) {
            editsByFile
                .computeIfAbsent(change.file(), f -> new ArrayList<>())
                .addAll(change.edits());
            uriByFile.putIfAbsent(change.file(), change.uri());
        }

        final var result = new LinkedHashMap<Path, List<FilePatcher.TextEdit>>();
        for (var fileEdits : editsByFile.entrySet()) {
            final var file = fileEdits.getKey();
            final var brokenReason = brokenFiles.get(file);
            if (brokenReason != null) {
                System.out.println(styled("-> skipped (" + brokenReason + ")", Styling::red));
                return null;
            }
            var patcher = patchers.get(file);
            if (patcher == null) {
                try {
                    final var fileState = state.fileState(file);
                    if (fileState != null) {
                        patcher = FilePatcher.readResuming(
                            file,
                            uriByFile.get(file),
                            forcedCharset,
                            fileState.edits(),
                            fileState.originalHash()
                        );

                        // The file is not the one the recorded fixes have been
                        // applied to, so what it currently holds is unknown:
                        // the coordinates of the report designate parts of a
                        // content which is gone, and the fixes of the previous
                        // runs would be applied a second time. Refuse to touch
                        // it rather than to patch it blindly.
                        if (patcher == null) {
                            brokenFiles.put(file, "does not match the recorded state");
                            diagnostics.add(
                                new Error(
                                    "\"" +
                                        file +
                                        "\" has been modified since the last run, so the fixes" +
                                        " already applied to it can no longer be identified." +
                                        " Regenerate the report from the current sources."
                                )
                            );
                            System.out.println(
                                styled(
                                    "-> skipped (does not match the recorded state)",
                                    Styling::red
                                )
                            );
                            return null;
                        }
                    } else {
                        patcher = FilePatcher.read(file, uriByFile.get(file), forcedCharset);
                    }
                } catch (IOException e) {
                    brokenFiles.put(file, "unreadable file");
                    diagnostics.add(new Error("Cannot read \"" + file + "\": " + reasonOf(e)));
                    System.out.println(styled("-> skipped (unreadable file)", Styling::red));
                    return null;
                }

                // When the report declares a digest for the file, check that
                // its content is the one the report has been created from.
                // When resuming, the digest is checked against the content
                // reconstructed from the state, before any previous patching.
                final var digest = digests.get(file);
                if (digest != null) {
                    if (!digest.equals(patcher.originalBytesDigest())) {
                        brokenFiles.put(file, "file changed since the report");
                        diagnostics.add(
                            new Error(
                                "\"" +
                                    file +
                                    "\" has changed since the report was created, its fixes" +
                                    " cannot be applied safely"
                            )
                        );
                        System.out.println(
                            styled("-> skipped (file changed since the report)", Styling::red)
                        );
                        return null;
                    }
                    verifiedFiles.add(file);
                }
                patchers.put(file, patcher);
            }
            try {
                result.put(file, patcher.toEdits(fileEdits.getValue()));
            } catch (FilePatcher.InvalidRegionException e) {
                diagnostics.add(
                    new Error("Invalid fix at " + fix.locationImage() + ": " + e.getMessage())
                );
                System.out.println(styled("-> skipped (" + e.getMessage() + ")", Styling::red));
                return null;
            }
        }
        return result;
    }

    /**
     * Prompt the user about the fix of the given rule and return their answer
     * among "y", "n", "a", "A" and "q". Each of them may also be answered with
     * its full name, and "h" displays what they all mean. Note that "a" and
     * "A" are the only answers whose case is meaningful.
     */
    private String ask(BufferedReader input, String ruleId) {
        while (true) {
            System.out.println(styled("Apply this fix? [y/n/a/A/q/h]", Styling::bold));
            final String line;
            try {
                line = input.readLine();
            } catch (IOException e) {
                return "eof";
            }
            if (line == null) {
                return "eof";
            }
            final var answer = line.trim();
            if (answer.equals("a") || answer.equals("A")) {
                return answer;
            }
            switch (answer.toLowerCase()) {
                case "y", "yes" -> {
                    return "y";
                }
                case "n", "no" -> {
                    return "n";
                }
                case "auto" -> {
                    return "a";
                }
                case "all" -> {
                    return "A";
                }
                case "q", "quit" -> {
                    return "q";
                }
                case "h", "help" -> printPromptHelp(ruleId);
                default -> System.out.println(
                    styled("Please answer y, n, a, A, q or h.", Styling::yellow)
                );
            }
        }
    }

    /**
     * Ask, when quitting, whether to keep the record of the fixes applied so
     * far, and return the answer. An exhausted input keeps it, as it is what
     * loses nothing.
     */
    private boolean askKeepState(BufferedReader input) {
        while (true) {
            System.out.println(
                styled("Keep the record of the fixes applied so far? [y/n/h]", Styling::bold)
            );
            final String line;
            try {
                line = input.readLine();
            } catch (IOException e) {
                return true;
            }
            if (line == null) {
                return true;
            }
            switch (line.trim().toLowerCase()) {
                case "y", "yes" -> {
                    return true;
                }
                case "n", "no" -> {
                    return false;
                }
                case "h", "help" -> {
                    System.out.println(
                        "  y, yes   keep \"" +
                            stateFile.getFileName() +
                            "\", so that running this report again resumes where you stopped"
                    );
                    System.out.println(
                        "  n, no    remove it: the sources keep the fixes of this run, but" +
                            " running this"
                    );
                    System.out.println("           report again would apply them a second time");
                    System.out.println("  h, help  display this help");
                }
                default -> System.out.println(styled("Please answer y, n or h.", Styling::yellow));
            }
        }
    }

    /** Remove the record of the fixes applied from the report, if there is one. */
    private void forgetState() {
        try {
            if (Files.deleteIfExists(stateFile)) {
                System.out.println("Removed \"" + stateFile + "\".");
            }
        } catch (IOException e) {
            diagnostics.add(
                new Warning("Cannot remove the patch state \"" + stateFile + "\": " + reasonOf(e))
            );
        }
    }

    /** Display what each answer to the prompt means. */
    private void printPromptHelp(String ruleId) {
        System.out.println("  y, yes   apply this fix");
        System.out.println("  n, no    skip this fix");
        System.out.println(
            "  a, auto  apply this fix and all the remaining ones of \"" + ruleId + "\""
        );
        System.out.println("  A, all   apply this fix and all the remaining ones, of every rule");
        System.out.println("  q, quit  skip this fix and all the remaining ones");
        System.out.println("  h, help  display this help");
    }

    /** Print the given unified diff lines, colored when ANSI is supported. */
    private void printDiff(List<String> diffLines) {
        for (var line : diffLines) {
            final Styling.StylingFunction style;
            if (line.startsWith("+++") || line.startsWith("---")) {
                style = Styling::bold;
            } else if (line.startsWith("+")) {
                style = Styling::green;
            } else if (line.startsWith("-")) {
                style = Styling::red;
            } else if (line.startsWith("@@")) {
                style = Styling::brightBlue;
            } else {
                style = null;
            }
            System.out.println(style == null ? line : styled(line, style));
        }
    }

    /** Style the given text with the given styling function, when ANSI is supported. */
    private String styled(String text, Styling.StylingFunction style) {
        return supportAnsi ? style.apply(text) : text;
    }

    // ----- Stubs inherited from BaseSubcommand ----

    @Override
    protected List<String> preprocessArguments(
        List<String> arguments,
        Map<String, String> polyglotOptions
    ) {
        throw new AssertionError("Should not reach here");
    }

    @Override
    protected void launch(Builder contextBuilder) {
        throw new AssertionError("Should not reach here");
    }

    @Override
    protected String getLanguageId() {
        throw new AssertionError("Should not reach here");
    }

    @Override
    protected void printHelp(OptionCategory maxCategory) {
        throw new AssertionError("Should not reach here");
    }
}
