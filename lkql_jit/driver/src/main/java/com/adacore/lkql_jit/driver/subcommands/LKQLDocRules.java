//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.driver.checker.Rule;
import com.adacore.lkql_jit.driver.checker.RuleRepository;
import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.TextReportCreator;
import com.adacore.lkql_jit.options.LKQLOptions;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.graalvm.collections.Pair;
import org.graalvm.polyglot.Context;
import picocli.CommandLine;

@CommandLine.Command(
    name = "doc-rules",
    description = "Generate rules documentation, in RST format"
)
public class LKQLDocRules implements Callable<Integer> {

    // ----- Attributes -----

    /** Pattern that matches the ".. param" and ".. skip_param" RST directives. */
    private static final Pattern PARAM_DIRECTIVE_MATCHER = Pattern.compile(
        ".. ((skip_)?param):: (.*)"
    );

    /** Set of accepted rule parameter types. */
    private static final Set<String> VALID_PARAM_TYPES = Set.of("bool", "int", "string", "list");

    /** Whether the current output support ANSI colors. */
    protected final boolean supportAnsi;

    @CommandLine.Parameters(
        description = "Any number of rules directories for which to generate documentation"
    )
    final List<Path> rulesDirs = new ArrayList<>();

    @CommandLine.Option(
        names = { "-O", "--output-dir" },
        description = "Output directory for generated RST files (default to local directory)"
    )
    final Path outputDir = Paths.get(".");

    @CommandLine.Option(names = { "-v", "--verbose" }, description = "Verbose mode.")
    boolean verbose;

    // ----- Constructor -----

    public LKQLDocRules() {
        this.supportAnsi = System.getenv("TERM") != null && System.console() != null;
    }

    // ----- Class methods -----

    private static String toMixedCase(String src) {
        return Arrays.stream(src.split("_"))
            .map(s -> s.substring(0, 1).toUpperCase() + s.substring(1))
            .collect(Collectors.joining("_"));
    }

    /** Get the name to display in doc for the provided rule. */
    private static String displayName(Rule rule) {
        return rule.displayName().equals(rule.name())
            ? toMixedCase(rule.name())
            : rule.displayName();
    }

    /** Get a formatted string corresponding to an RST heading named 'name'. */
    private static String rstHeading(String name, Character kind) {
        var heading = "``" + name + "``";
        return heading + "\n" + kind.toString().repeat(heading.length());
    }

    /** Get a formatted string for an RST anchor named 'name'. */
    private static String rstAnchor(String name) {
        return ".. _" + name + ":";
    }

    /** Get a formatted string for an RST index named 'name'. */
    private static String rstIndex(String name) {
        return ".. index:: " + name.replace(" ", "_");
    }

    /** Raise an exception about an error while documenting the specified rule. */
    private static void errorInDoc(Rule rule, String message) {
        throw new RuntimeException(
            "Error when generating the documentation for the rule \"" +
                rule.name() +
                "\" (" +
                message +
                ')'
        );
    }

    /** Return whether the rule is from category and subcategory. */
    private static boolean isFromCategory(Rule rule, String category, String subcategory) {
        return (rule.category().equals(category) && rule.subcategory().equals(subcategory));
    }

    /** Generate the RST documentation corresponding to the provided rule. */
    private static String toRST(Rule rule) {
        var docString = new StringBuilder();
        docString
            .append(rstAnchor(displayName(rule)))
            .append("\n\n")
            .append(rstHeading(displayName(rule), rule.subcategory().equals("Misc") ? '-' : '^'))
            .append("\n\n")
            .append(rstIndex(displayName(rule)))
            .append("\n\n")
            .append(getDoc(rule));

        if (rule.autoFix().isPresent()) {
            docString
                .append("\n\n")
                .append(".. admonition:: Auto-fix available\n")
                .append("\n")
                .append("   ")
                .append(rule.autoFixDescription());
        }

        docString.append("\n\n\n");
        return docString.toString();
    }

    /**
     * Process the rule documentation and return the result (or throw an error if the
     * documentation is missing something).
     */
    private static String getDoc(Rule rule) {
        // Create a map of parameters to document, keys are parameter names, and values are pairs
        // with their types and their default value.
        var paramsToDocument = new HashMap<String, Pair<Optional<String>, Optional<String>>>();
        for (int i = 1; i < rule.checker().parameterNames.length; i++) {
            var defaultValue = Optional.ofNullable(rule.checker().parameterDefaultValues[i]);
            paramsToDocument.put(
                rule.checker().parameterNames[i],
                Pair.create(
                    Optional.ofNullable(rule.checker().parameterTypes[i]),
                    defaultValue.map(v -> v.getSourceSection().getCharacters().toString())
                )
            );
        }

        // Replace all ".. param" directives in the documentation
        var doc = PARAM_DIRECTIVE_MATCHER.matcher(rule.checker().documentation).replaceAll(
            matchResult -> {
                var directiveName = matchResult.group(1);
                var paramName = matchResult.group(3);

                // Fetch the parameter declaration related to the name
                var relatedParam = paramsToDocument.remove(paramName);

                // Check that the documented parameter exists
                if (relatedParam == null) errorInDoc(rule, "Unknown parameter " + paramName);

                // Get type and default value of the parameter
                var maybeParamType = relatedParam.getLeft();
                var paramDefaultValue = relatedParam.getRight();

                // Now check that all information about the parameter are available
                if (maybeParamType.isEmpty()) errorInDoc(
                    rule,
                    "Missing type annotation for parameter " + paramName
                );
                var paramType = maybeParamType.get();

                // Check that the parameter type is valid
                if (!VALID_PARAM_TYPES.contains(paramType)) errorInDoc(
                    rule,
                    "Invalid type " + paramType + " for parameter " + paramName
                );

                // Now create the default value annotation
                var defaultValPrecision = paramDefaultValue
                    .map(d -> "(default: ``" + d + "``)")
                    .orElse("(no default value, this parameter is mandatory)");

                return directiveName.equals("param")
                    ? ("- *" +
                          toMixedCase(paramName) +
                          ": " +
                          paramType +
                          "* " +
                          defaultValPrecision)
                    : "";
            }
        );

        if (!paramsToDocument.isEmpty()) errorInDoc(
            rule,
            "Those parameters are missing a docstring " + paramsToDocument.keySet()
        );

        // Finally return the documentation
        return doc;
    }

    /**
     * Print the rules for the category named 'categoryName' in file 'file'. Also, print the RST
     * string 'header' as section header.
     */
    private static void printCategory(
        FileWriter file,
        List<Rule> rules,
        String categoryName,
        String header
    ) throws Exception {
        var title = categoryName + "-Related Rules";
        file.write(rstHeading(title, '=') + "\n\n");
        file.write(rstIndex(title) + "\n\n");
        file.write(header + "\n\n\n");

        var iter = rules.listIterator();
        while (iter.hasNext()) {
            var next = iter.next();
            if (isFromCategory(next, categoryName, "Misc")) {
                file.write(toRST(next));
                iter.remove();
            }
        }
    }

    /**
     * Print the rules for the subcategory named 'subcategoryName' (from 'categoryName') in file
     * 'file'. Also, print the RST string 'header' as section header.
     */
    private static void printSubcategory(
        FileWriter file,
        List<Rule> rules,
        String categoryName,
        String subcategoryName,
        String header
    ) throws Exception {
        file.write(rstAnchor(subcategoryName.replace(" ", "_")) + "\n\n");
        file.write(rstHeading(subcategoryName, '-') + "\n\n");
        file.write(rstIndex(subcategoryName + "-related rules") + "\n\n");
        file.write(header + "\n\n\n");

        var iter = rules.listIterator();
        while (iter.hasNext()) {
            var next = iter.next();
            if (isFromCategory(next, categoryName, subcategoryName)) {
                file.write(toRST(next));
                iter.remove();
            }
        }
    }

    // ----- Instance methods -----

    @Override
    public Integer call() throws Exception {
        // Create a new collector for diagnostics
        var diagnostics = new DiagnosticCollector();

        // Create a text report creator to display potential diagnostics
        var reporter = new TextReportCreator(System.out, supportAnsi);

        // Now create a context to get all rules
        var contextBuilder = Context.newBuilder(Constants.LKQL_ID)
            .allowAllAccess(true)
            .option("lkql.options", new LKQLOptions.Builder().build().toJson().toString());

        // Create a rule repository and populate it
        RuleRepository ruleRepository;
        try (var context = contextBuilder.build()) {
            if (verbose) System.out.println("Analysing rule files in directories: " + rulesDirs);
            ruleRepository = new RuleRepository(context, rulesDirs, diagnostics);

            // Check if some errors occurred during rules fetching
            if (diagnostics.hasError()) {
                diagnostics.createReport(reporter);
                return 1;
            }

            if (verbose) System.out.println(
                "Found " + ruleRepository.rules.size() + " rules for documentation."
            );
        }

        // Now create a list of all rules, sorted by name
        var rules = ruleRepository.rules
            .values()
            .stream()
            .sorted(Comparator.comparing(Rule::name))
            .collect(Collectors.toList());

        // Create the output directory if it doesn't exist
        if (!Files.exists(outputDir)) Files.createDirectories(outputDir);

        // Generate the list of rules.
        var listOfRules = new FileWriter(outputDir.resolve("list_of_rules.rst").toFile());

        listOfRules.write(
            """
            .. _List_of_Rules:

            **************************
            Alphabetical List of Rules
            **************************

            This section contains an alphabetized list of all the predefined
            GNATcheck rules.

            """
        );
        for (var r : rules) listOfRules.write("* :ref:`" + displayName(r) + "`\n");

        listOfRules.close();

        // Generate rules documentation. Warning: this will consume rules in
        // `checks` in the following category/subcategory order:
        //
        // * Style-related rules
        //     * Tasking-related rules
        //     * Object-Orientation related rules
        //     * Portability-related rules
        //     * Program Structure related rules
        //     * Programming Practice related rules
        //     * Readability-related rules
        // * Feature Usage Rules
        // * Metrics-related rules
        // * SPARK related rules

        var predefinedRules = new FileWriter(outputDir.resolve("predefined_rules.rst").toFile());

        predefinedRules.write(
            """
            .. _Predefined_Rules:

            ****************
            Predefined Rules
            ****************

            .. index:: Predefined Rules

            The description of the rules currently implemented in ``gnatcheck`` is
            given in this chapter.
            The rule identifier is used as a key for LKQL rule configuration objects (see
            :ref:`LKQL rule file<LKQL_options_file>`), and as first parameter of
            ``gnatcheck``'s ``+R`` or ``-R`` switches.

            Be aware that most of these rules apply to specialized coding
            requirements developed by individual users and may well not make sense in
            other environments. In particular, there are many rules that conflict
            with one another. Proper usage of gnatcheck involves selecting the rules
            you wish to apply by looking at your independently developed coding
            standards and finding the corresponding gnatcheck rules.

            Unless documentation is specifying some, rules don't have any parameters.

            If not otherwise specified, a rule does not do any check for the
            results of generic instantiations.

            GNATcheck's predefined rules' parameters may have the following types:

            *bool*
               The parameter represents a boolean value, toggling a rule behavior.
               In a LKQL rule file you have to associate a boolean value to the parameter
               name:

               .. code-block:: lkql

                  val rules = @{
                     My_Rule: {Bool_Param: true}
                  }

               To specify a boolean parameter through a ``+R`` option, you just have to provide
               the parameter's name to set it to true:

               .. code-block:: ada

                  +RMy_Rule:Bool_Param  -- 'Bool_Param' value is set to true

            *int*
               The parameter is an integer value.
               In a LKQL rule options file, you have to associate an integer value to the
               parameter name:

               .. code-block:: lkql

                  val rules = @{
                     My_Rule: {N: 5} # If the rule param is named 'N'
                  }

               To specify it with a ``+R`` option, you can write its value right after the
               rule name:

               .. code-block:: ada

                  +RMy_Rule:5  -- 'My_Rule' integer param is set to 5

            *string*
               The parameter value is a string, sometimes with formatting constraints.
               In a LKQL rule options file, you just have to provide a string value:

               .. code-block:: lkql

                  val rules = @{
                     My_Rule: {Str: "i_am_a_string"} # If the rule param is named 'Str'
                  }

               You can specify it through the ``+R`` option also by passing a string right
               after the rule name:

               .. code-block:: ada

                  +RMy_Rule:i_am_a_string  -- 'My_Rule' string param is set to "i_am_a_string"

            *list*
               The parameter value is a list of string.
               In a LKQL rule options file, you can use the LKQL list type to specify the
               parameter value:

               .. code-block:: lkql

                  val rules = @{
                     My_Rule: {Lst: ["One", "Two", "Three"]} # If the rule param is named 'Lst'
                  }

               Through the ``+R`` option, you can specify it as a collection of string
               parameters separated by commas:

               .. code-block:: ada

                  +RMy_Rule:One,Two,Three  -- 'My_Rule' string list param is set to ["One", "Two", "Three"]



            """
        );

        printCategory(
            predefinedRules,
            rules,
            "Style",
            """
            The rules in this section may be used to enforce various feature usages
            consistent with good software engineering, for example
            as described in Ada 95 Quality and Style.
            """
        );

        printSubcategory(
            predefinedRules,
            rules,
            "Style",
            "Tasking",
            """
            The rules in this subsection may be used to enforce various
            feature usages related to concurrency.
            """
        );

        printSubcategory(
            predefinedRules,
            rules,
            "Style",
            "Object Orientation",
            """
            The rules in this subsection may be used to enforce various
            feature usages related to Object-Oriented Programming.
            """
        );

        printSubcategory(
            predefinedRules,
            rules,
            "Style",
            "Portability",
            """
            The rules in this subsection may be used to enforce various
            feature usages that support program portability.
            """
        );

        printSubcategory(
            predefinedRules,
            rules,
            "Style",
            "Program Structure",
            """
            The rules in this subsection may be used to enforce feature usages
            related to program structure.
            """
        );

        printSubcategory(
            predefinedRules,
            rules,
            "Style",
            "Programming Practice",
            """
            The rules in this subsection may be used to enforce feature usages that
            relate to program maintainability.
            """
        );

        printSubcategory(
            predefinedRules,
            rules,
            "Style",
            "Readability",
            """
            The rules described in this subsection may be used to enforce feature usages
            that contribute towards readability.
            """
        );

        printCategory(
            predefinedRules,
            rules,
            "Feature",
            """
            The rules in this section can be used to enforce specific
            usage patterns for a variety of language features.
            """
        );

        printCategory(
            predefinedRules,
            rules,
            "Metrics",
            """
            The rules in this section can be used to enforce compliance with
            specific code metrics, by checking that the metrics computed for a program
            lie within user-specifiable bounds.

            The name of any metrics rule consists of the prefix ``Metrics_``
            followed by the name of the corresponding metric:
            ``Essential_Complexity``, ``Cyclomatic_Complexity``, or
            ``LSLOC``.
            (The 'LSLOC' acronym stands for 'Logical Source Lines Of Code'.)
            The meaning and the computed values of the metrics are
            the same as in *gnatmetric*.
            """
        );

        printCategory(
            predefinedRules,
            rules,
            "SPARK",
            """
            The rules in this section can be used to enforce
            compliance with the Ada subset allowed by the SPARK 2005 language.

            More recent versions of SPARK support these language constructs,
            so if you want to further restrict the SPARK constructs allowed
            in your coding standard, you can use some of the following rules.
            """
        );

        predefinedRules.close();

        // Finally, show rules that haven't been documented
        if (!rules.isEmpty()) {
            System.err.println("Error: " + rules.size() + " rules not documented!");
            for (var r : rules) System.out.println(r.name().toString());
        }

        return 0;
    }
}
