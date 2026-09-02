//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.patching;

import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.diagnostics.variants.Warning;
import de.jcup.sarif_2_1_0.SarifSchema210ImportExportSupport;
import de.jcup.sarif_2_1_0.model.Artifact;
import de.jcup.sarif_2_1_0.model.ArtifactLocation;
import de.jcup.sarif_2_1_0.model.Region;
import de.jcup.sarif_2_1_0.model.Replacement;
import de.jcup.sarif_2_1_0.model.Result;
import de.jcup.sarif_2_1_0.model.Run;
import de.jcup.sarif_2_1_0.model.SarifSchema210;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Loader of quick fixes from a SARIF report.
 *
 * <p>
 * Fixes are searched in the "results[].fixes[]" objects of every run of the
 * report. Only line/column based regions are supported, and when a result
 * carries several alternative fixes, only the first one is considered.
 */
public final class SarifFixLoader {

    /** Displayed identifier of a result which does not name its rule. */
    public static final String UNKNOWN_RULE = "<unknown rule>";

    /** Depth beyond which the URI bases of a report are considered circular. */
    private static final int MAX_BASE_DEPTH = 8;

    private SarifFixLoader() {}

    /**
     * The content loaded from a SARIF report: its quick fixes, in the order the
     * report lists them, the SHA-256 digest the report declares for the files
     * it analyzed, if any, and the identifier of every rule it mentions. A
     * digest allows the patcher to check that a file has not changed since the
     * report has been created.
     *
     * <p>
     * Rules are held apart from the fixes because a rule may have reported
     * something without providing any fix for it, in which case it appears
     * nowhere in the fixes, while it is still a rule of the report.
     */
    public record Report(List<CandidateFix> fixes, Map<Path, String> digests, Set<String> rules) {
        /**
         * Get the number of fixes each rule of the report provides, rules
         * providing none included, ordered by rule identifier.
         */
        public Map<String, Integer> fixesPerRule() {
            final var counts = new TreeMap<String, Integer>();
            rules.forEach(rule -> counts.put(rule, 0));
            fixes.forEach(fix -> counts.merge(fix.ruleId(), 1, Integer::sum));
            return counts;
        }
    }

    /**
     * Exception raised when an artifact URI designates no usable file. It is
     * unchecked, as the other failures of a malformed report are, so that it
     * reaches the guard of whichever part of the report is being loaded.
     */
    static final class UnresolvableUriException extends RuntimeException {

        UnresolvableUriException(String message) {
            super(message);
        }
    }

    /**
     * Load the quick fixes contained in the given SARIF report, as well as the
     * identifier of every rule it mentions, including the ones which provide no
     * fix at all.
     *
     * <p>
     * Artifact URIs are resolved as {@link #resolveArtifactUri} describes,
     * from the roots the report declares. Problems are reported through the
     * diagnostic collector: an unreadable report yields an empty value, and
     * invalid fixes are skipped with a warning.
     */
    public static Optional<Report> load(Path sarifFile, DiagnosticCollector diagnostics) {
        final SarifSchema210 report;
        try {
            report = new SarifSchema210ImportExportSupport().fromFile(sarifFile.toFile());
        } catch (IOException | RuntimeException e) {
            diagnostics.add(
                new Error("Cannot read the SARIF report \"" + sarifFile + "\": " + e.getMessage())
            );
            return Optional.empty();
        }

        final var fixes = new ArrayList<CandidateFix>();
        final var digests = new HashMap<Path, String>();
        final var rules = new TreeSet<String>();
        for (var run : orEmpty(report.getRuns())) {
            // A JSON array may hold nothing where an object is expected, and
            // there is nothing to load from such an entry. Only the entries
            // which carry no information at all are skipped this way: deeper
            // ones are left to the guards below, which reject the fix holding
            // them instead of applying what is left of it.
            if (run == null) {
                diagnostics.add(new Warning("Skipping an empty run of the report"));
                continue;
            }
            final var bases = loadBases(run, diagnostics);
            for (var artifact : orEmpty(run.getArtifacts())) {
                try {
                    loadArtifactDigest(artifact, bases, digests);
                } catch (RuntimeException e) {
                    diagnostics.add(
                        new Warning("Skipping an unusable artifact of the report: " + reasonOf(e))
                    );
                }
            }
            for (var result : orEmpty(run.getResults())) {
                if (result == null) {
                    diagnostics.add(new Warning("Skipping an empty result of the report"));
                    continue;
                }

                // Nothing else in a result is trustworthy enough to be handled
                // outside of the guard below: a single unusable result must not
                // bring the whole command down
                final var ruleId = Optional.ofNullable(result.getRuleId()).orElse(UNKNOWN_RULE);
                Optional<CandidateFix> loaded;
                try {
                    loaded = loadResultFix(result, bases, diagnostics);
                } catch (RuntimeException e) {
                    diagnostics.add(
                        new Warning(
                            "Skipping an unusable fix of \"" + ruleId + "\": " + reasonOf(e)
                        )
                    );
                    loaded = Optional.empty();
                }
                loaded.ifPresent(fixes::add);

                // Record every rule the report mentions, so that the ones
                // providing no fix at all are known too
                rules.add(ruleId);
            }
        }
        return Optional.of(new Report(fixes, digests, rules));
    }

    // ----- Internal methods -----

    /** Record the SHA-256 digest the report declares for the given artifact, if any. */
    private static void loadArtifactDigest(
        Artifact artifact,
        Map<String, Path> bases,
        Map<Path, String> digests
    ) {
        final var location = artifact.getLocation();
        final var hashes = artifact.getHashes();
        if (location == null || location.getUri() == null || hashes == null) {
            return;
        }
        final var properties = hashes.getAdditionalProperties();
        if (properties == null) {
            return;
        }
        final var file = resolveArtifactUri(location, bases);
        for (var entry : properties.entrySet()) {
            if ("sha-256".equalsIgnoreCase(entry.getKey()) && entry.getValue() != null) {
                digests.put(file, entry.getValue().toLowerCase());
            }
        }
    }

    /** Load the fix attached to the given SARIF result, if any. */
    private static Optional<CandidateFix> loadResultFix(
        Result result,
        Map<String, Path> bases,
        DiagnosticCollector diagnostics
    ) {
        final var fixes = result.getFixes();
        if (fixes == null || fixes.isEmpty()) {
            return Optional.empty();
        }

        final var ruleId = Optional.ofNullable(result.getRuleId()).orElse(UNKNOWN_RULE);
        final var message = Optional.ofNullable(result.getMessage())
            .map(m -> m.getText())
            .orElse("<no message>");
        final var locationImage = locationImage(result);
        final var fixImage = "fix for \"" + ruleId + "\" at " + locationImage;

        if (fixes.size() > 1) {
            diagnostics.add(
                new Warning(
                    "Result carries several alternative fixes, only the first one is" +
                        " considered (" +
                        fixImage +
                        ")"
                )
            );
        }
        final var fix = fixes.iterator().next();

        final var changes = new ArrayList<CandidateFix.FileChange>();
        for (var change : orEmpty(fix.getArtifactChanges())) {
            final var artifactLocation = change.getArtifactLocation();
            if (artifactLocation == null || artifactLocation.getUri() == null) {
                diagnostics.add(new Warning("Skipping " + fixImage + ": missing artifact URI"));
                return Optional.empty();
            }
            final Path file;
            try {
                file = resolveArtifactUri(artifactLocation, bases);
            } catch (UnresolvableUriException e) {
                // Caught here rather than left to the guard of the whole
                // result, so that the fix the URI belongs to is named
                diagnostics.add(new Warning("Skipping " + fixImage + ": " + e.getMessage()));
                return Optional.empty();
            }

            final var edits = new ArrayList<CandidateFix.RegionEdit>();
            for (var replacement : orEmpty(change.getReplacements())) {
                final var edit = toRegionEdit(replacement);
                if (edit.isEmpty()) {
                    diagnostics.add(
                        new Warning("Skipping " + fixImage + ": unsupported replacement shape")
                    );
                    return Optional.empty();
                }
                edits.add(edit.get());
            }
            if (edits.isEmpty()) {
                diagnostics.add(new Warning("Skipping " + fixImage + ": no replacement"));
                return Optional.empty();
            }
            changes.add(new CandidateFix.FileChange(file, artifactLocation.getUri(), edits));
        }
        if (changes.isEmpty()) {
            diagnostics.add(new Warning("Skipping " + fixImage + ": no artifact change"));
            return Optional.empty();
        }

        return Optional.of(new CandidateFix(ruleId, message, locationImage, changes));
    }

    /**
     * Convert a SARIF replacement to a region edit. Only text insertions and
     * line/column based regions with an explicit end column are supported.
     */
    private static Optional<CandidateFix.RegionEdit> toRegionEdit(Replacement replacement) {
        final Region region = replacement.getDeletedRegion();
        if (region == null || region.getStartLine() == null) {
            return Optional.empty();
        }
        final int startLine = region.getStartLine();
        final int startColumn = region.getStartColumn() == null ? 1 : region.getStartColumn();
        final int endLine = region.getEndLine() == null ? startLine : region.getEndLine();
        if (region.getEndColumn() == null) {
            return Optional.empty();
        }
        final int endColumn = region.getEndColumn();

        final var content = replacement.getInsertedContent();
        if (content != null && content.getText() == null) {
            return Optional.empty();
        }
        final var insertedText = content == null ? "" : content.getText();

        // The snippet of the deleted region, when the report provides one,
        // tells which text this edit is expected to delete
        final var snippet = region.getSnippet();
        final var expectedText = snippet == null ? null : snippet.getText();

        return Optional.of(
            new CandidateFix.RegionEdit(
                startLine,
                startColumn,
                endLine,
                endColumn,
                insertedText,
                expectedText
            )
        );
    }

    /** Get a "file:line:column" image of the first location of the given result. */
    private static String locationImage(Result result) {
        final var locations = result.getLocations();
        if (locations == null || locations.isEmpty()) {
            return "<unknown location>";
        }
        final var physicalLocation = locations.get(0).getPhysicalLocation();
        if (physicalLocation == null || physicalLocation.getArtifactLocation() == null) {
            return "<unknown location>";
        }
        final var uri = physicalLocation.getArtifactLocation().getUri();
        final var name = uri == null ? "<unknown file>" : printable(uri);
        final var region = physicalLocation.getRegion();
        if (region == null || region.getStartLine() == null) {
            return name;
        }
        final var column = region.getStartColumn() == null ? 1 : region.getStartColumn();
        return name + ":" + region.getStartLine() + ":" + column;
    }

    /**
     * Resolve the roots a run declares through "originalUriBaseIds", by
     * identifier. A root may itself be expressed relatively to another one,
     * which is followed, up to a depth beyond which the declarations are
     * considered to refer to each other.
     */
    private static Map<String, Path> loadBases(Run run, DiagnosticCollector diagnostics) {
        final var declared = run.getOriginalUriBaseIds();
        if (declared == null || declared.getAdditionalProperties() == null) {
            return Map.of();
        }
        final var entries = declared.getAdditionalProperties();
        final var result = new HashMap<String, Path>();
        for (var id : entries.keySet()) {
            final var base = resolveBase(id, entries, 0);
            if (base == null) {
                diagnostics.add(
                    new Warning("Ignoring the URI base \"" + id + "\": it cannot be resolved")
                );
            } else {
                result.put(id, base);
            }
        }
        return result;
    }

    /** Resolve one declared root, following the one it is relative to if any. */
    private static Path resolveBase(String id, Map<String, ArtifactLocation> entries, int depth) {
        final var location = entries.get(id);
        if (depth > MAX_BASE_DEPTH || location == null || location.getUri() == null) {
            return null;
        }
        final var path = Path.of(rawPathOf(location.getUri()));
        final var parentId = location.getUriBaseId();
        if (parentId == null) {
            return path.isAbsolute() ? canonical(path) : null;
        }
        final var parent = resolveBase(parentId, entries, depth + 1);
        return parent == null ? null : canonical(parent.resolve(path));
    }

    /**
     * Resolve a SARIF artifact location to an absolute normalized path.
     *
     * <p>
     * A relative URI is resolved from the root its "uriBaseId" names among the
     * ones the report declares in "originalUriBaseIds", and that root must be
     * declared. It may well lead out of it: a root is where the paths of a
     * report are shortened from, not a boundary of what it covers, and the
     * sources of a project may live outside of its directory. A URI naming no
     * root has nothing to be resolved from, so it is only usable when it is
     * absolute, in which case it designates its file by itself.
     *
     * @throws UnresolvableUriException when the location designates no usable
     *     file, with a message telling which of these expectations it breaks.
     */
    static Path resolveArtifactUri(ArtifactLocation location, Map<String, Path> bases) {
        final var uri = printable(location.getUri());
        final var path = Path.of(rawPathOf(location.getUri()));
        final var baseId = location.getUriBaseId();
        if (baseId == null) {
            if (path.isAbsolute()) {
                return canonical(path);
            }
            throw new UnresolvableUriException("\"" + uri + "\" is relative but names no URI base");
        }
        final var root = bases.get(baseId);
        if (root == null) {
            throw new UnresolvableUriException(
                "\"" + uri + "\" names \"" + baseId + "\", which is not a usable URI base"
            );
        }

        // A URI naming a root is expected to be relative to it. An absolute
        // one designates its file on its own, whatever the root says.
        return canonical(path.isAbsolute() ? path : root.resolve(path));
    }

    /**
     * Get the path a SARIF URI denotes, as a string. Absolute "file:" URIs
     * are decoded; other values are treated as plain paths.
     */
    private static String rawPathOf(String uri) {
        var rawPath = uri;
        try {
            final var parsed = new URI(uri);
            if ("file".equalsIgnoreCase(parsed.getScheme())) {
                try {
                    return Path.of(parsed).toString();
                } catch (RuntimeException e) {
                    // "file:" URI shape not convertible to a path, such as an
                    // opaque "file:relative.adb" or an authority component:
                    // use its decoded content as a plain path
                    rawPath = parsed.getPath() != null
                        ? parsed.getPath()
                        : parsed.getSchemeSpecificPart();
                }
            } else if (parsed.getScheme() == null && parsed.getPath() != null) {
                rawPath = parsed.getPath();
            }
        } catch (URISyntaxException e) {
            // Not a valid URI, for example a "file:" URI containing unencoded
            // characters such as spaces: strip the scheme prefix if any and
            // treat the remaining value as a plain path
            if (rawPath.regionMatches(true, 0, "file://", 0, 7)) {
                rawPath = rawPath.substring(7);
            } else if (rawPath.regionMatches(true, 0, "file:", 0, 5)) {
                rawPath = rawPath.substring(5);
            }
        }

        return rawPath;
    }

    /**
     * Get the canonical form of the given path: absolute with symbolic links
     * resolved when the path exists, absolute and normalized otherwise. Used
     * for all path comparisons, so that the same file is recognized whatever
     * the spelling of its path, be it through "..", a symbolic link, or the
     * root of the report it comes from.
     */
    public static Path canonical(Path path) {
        try {
            return path.toRealPath();
        } catch (IOException e) {
            return path.toAbsolutePath().normalize();
        }
    }

    /**
     * Get a message describing the given failure. The message of an invalid
     * path is the path itself, which says nothing about what is wrong with
     * it, and a failure may carry no message at all.
     */
    private static String reasonOf(RuntimeException e) {
        return switch (e) {
            case InvalidPathException i -> "invalid path \"" + printable(i.getInput()) + "\"";
            case NullPointerException _ -> "incomplete location";
            default -> e.getMessage() == null ? e.getClass().getSimpleName() : e.getMessage();
        };
    }

    /**
     * Get the given text with its control characters escaped. What a report
     * holds ends up in diagnostics, and a path may hold characters which
     * would otherwise go unnoticed, or garble the output.
     */
    private static String printable(String text) {
        final var result = new StringBuilder();
        for (var character : text.toCharArray()) {
            if (character < 0x20 || character == 0x7F) {
                result.append(String.format("\\u%04X", (int) character));
            } else {
                result.append(character);
            }
        }
        return result.toString();
    }

    /** Get the given collection, or an empty one when null. */
    private static <T> Collection<T> orEmpty(Collection<T> collection) {
        return collection == null ? List.of() : collection;
    }
}
