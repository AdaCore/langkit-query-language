//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.checker;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.variants.RawMessage;
import com.adacore.lkql_jit.driver.diagnostics.variants.Warning;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLCallable;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;

/** This class is used to fetch and store all LKQL rules available in a given environment. */
public final class RuleRepository {

    // ----- Attributes -----

    /** A map associating rules to their names. */
    private final Map<String, Rule> rules;

    // ----- Constructors -----

    /**
     * Create a new rule repository and fill it by loading all available LKQL rules in the provided
     * directories.
     *
     * @param context The polyglot execution context that is going to be used for rule loading.
     * @param searchingDirs All directories that are going to be searched in for LKQL files.
     */
    public RuleRepository(
        Context context,
        List<Path> searchingDirs,
        DiagnosticCollector diagnostics
    ) {
        this.rules = new HashMap<>();

        // First fetch all LKQL files
        var lkqlFiles = allLKQLFiles(searchingDirs);

        // Then for each one, execute it and search for checking functions
        for (var lkqlFile : lkqlFiles) {
            try {
                Source source = Source.newBuilder(Constants.LKQL_ID, lkqlFile.toFile()).build();
                LKQLBaseNamespace namespace = context.eval(source).as(LKQLBaseNamespace.class);
                for (var lkqlValue : namespace.asMap().values()) {
                    if (lkqlValue instanceof LKQLCallable lkqlCallable) {
                        ruleFromCallable(lkqlCallable).ifPresent(r -> this.addRule(r, diagnostics));
                    }
                }
            } catch (IOException e) {
                // This shouldn't happen since we ensured all LKQL files are readable
                throw new RuntimeException(e);
            } catch (PolyglotException e) {
                // When an error occurs in the execution of the LKQL file, store it in diagnostics
                diagnostics.add(new RawMessage(e.getMessage()));
            }
        }
    }

    /** Return an ordered set of all readable LKQL files contained in provided directories. */
    private static Set<Path> allLKQLFiles(List<Path> searchingDirs) {
        Set<Path> res = new TreeSet<>();

        // Fetch all LKQL files from the searching dirs
        for (var dir : searchingDirs.stream().filter(Files::isDirectory).toList()) {
            try (var files = Files.list(dir)) {
                res.addAll(
                    files
                        .filter(
                            f ->
                                f.toString().endsWith(Constants.LKQL_EXTENSION) &&
                                Files.isRegularFile(f) &&
                                Files.isReadable(f)
                        )
                        .toList()
                );
            } catch (IOException e) {
                // Here we want the application to crash when a directory is not readable
                throw new RuntimeException(e);
            }
        }

        return res;
    }

    /**
     * Create a rule object from a callable LKQL value if the latter represents a checking function.
     */
    private static Optional<Rule> ruleFromCallable(LKQLCallable callable) {
        // Search for a "check" annotation on the callable
        var maybeCheckAnnotation = Arrays.stream(callable.annotations)
            .filter(
                a ->
                    a.name().equals(Constants.ANNOTATION_NODE_CHECK) ||
                    a.name().equals(Constants.ANNOTATION_UNIT_CHECK)
            )
            .findFirst();

        // If there is a "check" annotation on the callable
        if (maybeCheckAnnotation.isPresent()) {
            // Get the check annotation
            var annotation = maybeCheckAnnotation.get();

            // Start by getting all required arguments for the new rule
            var allArguments = new HashMap<>(annotation.namedArguments());
            for (int i = 0; i < annotation.positionalArguments().size(); i++) {
                allArguments.put(
                    Constants.CHECKER_PARAMETER_NAMES[i],
                    annotation.positionalArguments().get(i)
                );
            }

            // Set manual default value for some arguments
            allArguments.putIfAbsent("message", callable.name);
            allArguments.putIfAbsent("help", callable.name);

            // Then fill all missing arguments with their default value
            for (int i = 0; i < Constants.CHECKER_PARAMETER_NAMES.length; i++) {
                allArguments.putIfAbsent(
                    Constants.CHECKER_PARAMETER_NAMES[i],
                    Constants.CHECKER_PARAMETER_DEFAULT_VALUES[i]
                );
            }

            // Get the remediation level
            var remediation = switch ((String) allArguments.get("remediation")) {
                case "EASY" -> Rule.Remediation.EASY;
                case "MAJOR" -> Rule.Remediation.MAJOR;
                default -> Rule.Remediation.MEDIUM;
            };

            // Finally return the new rule object
            return Optional.of(
                new Rule(
                    annotation.name().equals(Constants.ANNOTATION_NODE_CHECK)
                        ? Rule.Kind.NODE
                        : Rule.Kind.UNIT,
                    callable.name,
                    callable,
                    Optional.ofNullable((LKQLCallable) allArguments.get("auto_fix")),
                    (String) allArguments.get("message"),
                    (String) allArguments.get("help"),
                    (boolean) allArguments.get("follow_generic_instantiations"),
                    (String) allArguments.get("category"),
                    (String) allArguments.get("subcategory"),
                    remediation,
                    (long) allArguments.get("execution_cost"),
                    (boolean) allArguments.get("parametric_exemption"),
                    (String) allArguments.get("target")
                )
            );
        }
        return Optional.empty();
    }

    /**
     * Internal helper to add a rule to this repository and check if one with the same name already
     * exists.
     */
    private void addRule(Rule rule, DiagnosticCollector diagnostics) {
        var previousRule = rules.put(rule.name().toLowerCase(), rule);
        if (previousRule != null) {
            diagnostics.add(
                new Warning(
                    "A checker named \"" +
                        rule.name() +
                        "\" from: " +
                        previousRule
                            .checker()
                            .getDeclarationLocation()
                            .get()
                            .getSource()
                            .getPath() +
                        ", has been replaced by the one from: " +
                        rule.checker().getDeclarationLocation().get().getSource().getPath() +
                        ". Note: checkers should have unique names."
                )
            );
        }
    }

    // ----- Instance methods -----

    /** Get a rule by its name, if it exists. */
    public Optional<Rule> getRuleByName(String name) {
        return Optional.ofNullable(this.rules.get(name));
    }
}
