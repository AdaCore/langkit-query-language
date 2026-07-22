//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import static com.adacore.liblkqllang.Liblkqllang.*;

import com.adacore.lkql_jit.Constants;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import picocli.CommandLine;

@CommandLine.Command(
    name = "doc-rules",
    description = "Generate rules documentation, in RST format"
)
public class LKQLDocRules implements Callable<Integer> {

    /** Pattern that matches the ".. param" and ".. skip_param" RST directives. */
    private static final Pattern PARAM_DIRECTIVE_MATCHER = Pattern.compile(
        ".. ((skip_)?param):: (.*)"
    );

    /** Set of accepted rule parameter types. */
    private static final Set<String> VALID_PARAM_TYPES = Set.of("bool", "int", "string", "list");

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

    private static String toMixedCase(String src) {
        return Arrays.stream(src.split("_"))
            .map(s -> s.substring(0, 1).toUpperCase() + s.substring(1))
            .collect(Collectors.joining("_"));
    }

    /**
     * Return whether `unit` contains a LKQL checker (assuming an AnalysisUnit contains only one
     * checker).
     *
     * @return The corresponding `FunDecl` if a checker is found, null otherwise.
     */
    private static FunDecl isCheck(AnalysisUnit unit) {
        for (var fun : unit
            .getRoot()
            .walk()
            .filter(n -> n instanceof FunDecl)
            .map(f -> (FunDecl) f)
            .toList()) {
            var ann = fun.fAnnotation();
            if (
                ann != null && !ann.isNone() && ann.fName().pSym().text.endsWith("check")
            ) return fun;
        }
        return null;
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

    /** Convert the LkqlNode 'literal' to RST (simply remove the leading '|" ' characters). */
    private static String docStringLiteralToRST(LkqlNode literal) {
        var line = literal.getText();
        return line.substring(Math.min(3, line.length()));
    }

    /** Object to represent a LKQL rule for easier documentation generation. */
    private record Rule(FunDecl check, String name, String category, String subcategory) implements
        Comparable<Rule> {
        public Rule(FunDecl check) {
            this(
                check,
                getAnnotationArgument(check, "rule_name").orElse(
                    toMixedCase(check.fName().pSym().text)
                ),
                getAnnotationArgument(check, "category").orElse(""),
                getAnnotationArgument(check, "subcategory").orElse("")
            );
        }

        /** Get the argument of annotation 'name' if it exists, empty string otherwise. */
        private static Optional<String> getAnnotationArgument(FunDecl check, String name) {
            var ann = check.fAnnotation();
            if (!ann.isNone()) {
                var arg = ann.pArgWithName(Symbol.create(name));
                if (!arg.isNone() && arg.pExpr() instanceof StringLiteral) {
                    var raw = arg.pExpr().getText();
                    return Optional.of(raw.substring(1, raw.length() - 1));
                }
            }
            return Optional.empty();
        }

        /** When compared, rules are sorted by names. */
        @Override
        public int compareTo(Rule other) {
            return this.name.compareToIgnoreCase(other.name);
        }

        /** Generate the RST documentation corresponding to this rule. */
        public String toRST() {
            var docString = new StringBuilder();
            docString
                .append(rstAnchor(this.name))
                .append("\n\n")
                .append(rstHeading(this.name, subcategory.isEmpty() ? '-' : '^'))
                .append("\n\n")
                .append(rstIndex(this.name))
                .append("\n\n")
                .append(getDoc())
                .append("\n\n\n");
            return docString.toString();
        }

        /**
         * Process the rule documentation and return the result (or throw an error if the
         * documentation is missing something).
         */
        private String getDoc() {
            // Create a map with all parameters of the rule. We skip the first parameter because
            // it is the object for the rule to analyze, so it's not part of the rule
            // configuration.
            var ruleParams = Arrays.stream(check.fFunExpr().fParameters().children())
                .map(p -> (ParameterDecl) p)
                .toList();
            Map<String, ParameterDecl> paramsMap = ruleParams.isEmpty()
                ? Map.of()
                : ruleParams
                      .subList(1, ruleParams.size())
                      .stream()
                      .collect(Collectors.toMap(p -> p.fParamIdentifier().getText(), p -> p));

            // Fetch the rule documentation
            var doc = switch (check.pDoc()) {
                case StringLiteral s -> docStringLiteralToRST(s);
                case BlockStringLiteral bsl -> Arrays.stream(bsl.fDocs().children())
                    .map(LKQLDocRules::docStringLiteralToRST)
                    .collect(Collectors.joining("\n"));
                default -> throw new RuntimeException("Invalid documentation " + check.pDoc());
            };

            // Now replace all ".. param" directives in the documentation
            doc = PARAM_DIRECTIVE_MATCHER.matcher(doc).replaceAll(matchResult -> {
                var directiveName = matchResult.group(1);
                var paramName = matchResult.group(3);

                // Fetch the parameter declaration related to the name
                var relatedParam = paramsMap.remove(paramName);

                // Now check that all information about the parameter are available
                if (relatedParam == null) errorInDoc("Unknown parameter " + paramName);
                if (relatedParam.fTypeAnnotation().isNone()) errorInDoc(
                    "Missing type annotation for parameter " + paramName
                );

                // Check that the parameter type is valid
                var paramType = relatedParam.fTypeAnnotation().getText();
                if (!VALID_PARAM_TYPES.contains(paramType)) errorInDoc(
                    "Invalid type " + paramType + " for parameter " + paramName
                );

                // Now create the default value annotation
                var defaultValPrecision = relatedParam.fDefaultExpr().isNone()
                    ? "(no default value, this parameter is mandatory)"
                    : "(default: ``" + relatedParam.fDefaultExpr().getText() + "``)";

                return directiveName.equals("param")
                    ? ("- *" +
                          toMixedCase(paramName) +
                          ": " +
                          paramType +
                          "* " +
                          defaultValPrecision)
                    : "";
            });

            if (!paramsMap.isEmpty()) errorInDoc(
                "Those parameters are missing a docstring " + paramsMap
            );

            // Finally return the documentation
            return doc;
        }

        private void errorInDoc(String message) {
            throw new RuntimeException(
                "Error when generating the documentation for the rule \"" +
                    name +
                    "\" (" +
                    check.fullSlocImage() +
                    "): " +
                    message
            );
        }

        /** Return whether this rule is from category 'category' and subcategory 'subcategory'. */
        public Boolean isFromCategory(String category, String subcategory) {
            return (this.category.equals(category) && this.subcategory.equals(subcategory));
        }
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
            if (next.isFromCategory(categoryName, "")) {
                file.write(next.toRST());
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
            if (next.isFromCategory(categoryName, subcategoryName)) {
                file.write(next.toRST());
                iter.remove();
            }
        }
    }

    @Override
    public Integer call() throws Exception {
        var context = AnalysisContext.create();

        if (verbose) System.out.println("Analysing rule files in directories: " + rulesDirs);

        // Get all lkql files from directories to analyze.
        var ruleDirectoryFiles = new ArrayList<Path>();
        for (var dir : rulesDirs) {
            try (var files = Files.list(dir.toAbsolutePath())) {
                ruleDirectoryFiles.addAll(
                    files
                        .filter(
                            p ->
                                Files.isReadable(p) &&
                                p.toString().endsWith(Constants.LKQL_EXTENSION)
                        )
                        .toList()
                );
            }
        }

        var units = new ArrayList<AnalysisUnit>();

        // Parse all rule files.
        for (var ruleFile : ruleDirectoryFiles) {
            var unit = context.getUnitFromFile(ruleFile.toAbsolutePath().toString());
            if (verbose) System.out.println(" * " + unit.getFileName());

            if (unit.getDiagnostics().length > 0) {
                System.err.println("Error while parsing \"" + unit.getFileName() + "\":");
                for (var diag : unit.getDiagnostics()) System.err.println(diag);
            } else units.add(unit);
        }

        // Create rules objects, only keep check/unit_check FunDecls. We need to
        // use Collectors.toList() here instead of a direct call to toList()
        // because we rely on the fact that the list is mutable for the
        // subsequent calls to printCategory/printSubcategory (mostly for
        // performance).
        var rules = units
            .stream()
            .map(LKQLDocRules::isCheck)
            .filter(Objects::nonNull)
            .map(Rule::new)
            .collect(Collectors.toList());

        if (verbose) System.out.println("Found " + rules.size() + " rules for documentation.");

        // Sort the rules alphabetically before generating documentation.
        Collections.sort(rules);

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
        for (var r : rules) listOfRules.write("* :ref:`" + r.name + "`\n");

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

        if (!rules.isEmpty()) {
            System.err.println("Error: " + rules.size() + " rules not documented!");
            for (var r : rules) System.out.println(r.toString());
        }

        return 0;
    }
}
