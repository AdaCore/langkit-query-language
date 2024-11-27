//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import static com.adacore.liblkqllang.Liblkqllang.*;

import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import picocli.CommandLine;

@CommandLine.Command(
        name = "doc-rules",
        description = "Generate rules documentation, in RST format")
public class LKQLDocRules implements Callable<Integer> {
    @CommandLine.Parameters(
            description = "Any number of rules directories for which to generate documentation")
    private List<File> rulesDirs = new ArrayList<File>();

    @CommandLine.Option(
            names = {"-O", "--output-dir"},
            description = "Output directory for generated RST files (default to local directory)")
    private File outputDir = new File(".");

    @CommandLine.Option(
            names = {"-v", "--verbose"},
            description = "Verbose mode.")
    private boolean verbose;

    /**
     * Helper for findAll. Visit all children of 'node', calling 'cons' on each of them. TODO: Hoist
     * in Java bindings
     */
    private static void visitChildren(LkqlNode node, Consumer<LkqlNode> cons) {
        if (node == null || node.isNone()) {
            return;
        }

        for (var c : node.children()) {
            if (c != null && !c.isNone()) {
                cons.accept(c);
                visitChildren(c, cons);
            }
        }
    }

    /**
     * Helper for refactor writers: Find all nodes that are children of root and which satisfies the
     * predicate 'pred' TODO: Hoist in Java bindings
     */
    public static List<LkqlNode> findAll(LkqlNode root, Predicate<LkqlNode> pred) {
        var result = new ArrayList<LkqlNode>();
        visitChildren(
                root,
                (c) -> {
                    if (pred.test(c)) {
                        result.add(c);
                    }
                });
        return result;
    }

    /**
     * Internal method: return whether unit contains a LKQL checker (assuming an AnalysisUnit
     * contains only one checker).
     *
     * @return The corresponding FunDecl if a check is found, null otherwise.
     */
    private static FunDecl isCheck(AnalysisUnit unit) {
        final LkqlNode root = unit.getRoot();

        for (var fun : findAll(root, (n) -> n instanceof FunDecl)) {
            final DeclAnnotation ann = ((FunDecl) fun).fAnnotation();
            if (ann != null && !ann.isNone() && ann.fName().pSym().text.endsWith("check"))
                return (FunDecl) fun;
        }

        return null;
    }

    /** Get a formatted string corresponding to a RST heading named 'name'. */
    private static String rstHeading(String name, Character kind) {
        final String heading = "``" + name + "``";
        return heading + "\n" + kind.toString().repeat(heading.length());
    }

    /** Get a formatted string for a RST anchor named 'name'. */
    private static String rstAnchor(String name) {
        return ".. _" + name + ":";
    }

    /** Get a formatted string for a RST index named 'name'. */
    private static String rstIndex(String name) {
        return ".. index:: " + name.replace(" ", "_");
    }

    /** Convert the LkqlNode 'literal' to RST (simply remove the leading '|" ' chararters). */
    private static String docStringLiteralToRST(LkqlNode literal) {
        final String line = literal.getText();
        return line.substring(Math.min(3, line.length()));
    }

    /** Object to represent a LKQL rule for easier documentation generation. */
    private record Rule(FunDecl check, String name, String category, String subcategory)
            implements Comparable<Rule> {

        public Rule(FunDecl check) {
            this(
                    check,
                    getRuleName(check),
                    getAnnotationArgument(check, "category"),
                    getAnnotationArgument(check, "subcategory"));
        }

        private static String getRuleName(FunDecl check) {
            // Format the rule name. The rule name comes either verbatim from the 'rule_name'
            // annotation's argument, or from the checker's own FunDecl name, reformatted in Ada
            // casing.
            String name = getAnnotationArgument(check, "rule_name");
            if (name == "") {
                name = check.fName().pSym().text;
                name =
                        Arrays.stream(name.split("[_]"))
                                .map(s -> s.substring(0, 1).toUpperCase() + s.substring(1))
                                .collect(Collectors.joining("_"));
            }
            return name;
        }

        /** Get the argument of annotation 'name' if it exists, empty string otherwise. */
        private static String getAnnotationArgument(FunDecl check, String name) {
            var ann = check.fAnnotation();

            if (ann != null && !ann.isNone() && ann.fName().pSym().text.endsWith("check")) {
                var arg = ann.pArgWithName(Symbol.create(name));
                if (!arg.isNone() && arg.pExpr() instanceof StringLiteral) {
                    var raw = arg.pExpr().getText();
                    return raw.substring(1, raw.length() - 1);
                }
            }
            return "";
        }

        /** When compared, rules are sorted by names. */
        @Override
        public int compareTo(Rule other) {
            return this.name.compareToIgnoreCase(other.name);
        }

        /** Generate the RST code corresponding to this rule. */
        public String toRST() {
            StringBuilder docString = new StringBuilder(500);

            docString.append(rstAnchor(this.name) + "\n\n");
            docString.append(rstHeading(this.name, subcategory.isEmpty() ? '-' : '^') + "\n\n");
            docString.append(rstIndex(this.name) + "\n\n");

            // Get the LkqlNode documentation node associated to this rule.
            var doc = this.check.pDoc();
            if (doc instanceof StringLiteral) {
                docString.append(docStringLiteralToRST(doc) + "\n");
            } else if (doc instanceof BlockStringLiteral) {
                for (var subBlocks : ((BlockStringLiteral) doc).fDocs().children()) {
                    docString.append(docStringLiteralToRST(subBlocks) + "\n");
                }
            } else {
                System.out.println(
                        "Warning: wrong or missing documentation for "
                                + this.name
                                + " (doc_node: "
                                + doc
                                + ")");
            }

            docString.append("\n\n\n");

            return docString.toString();
        }

        /** Return whether this rule is from category 'category' and subcategory 'subcategory'. */
        public Boolean isFromCategory(String category, String subcategory) {
            return this.category.equals(category) && this.subcategory.equals(subcategory);
        }
    }

    /**
     * Print the rules for the category named 'categoryName' in file 'file'. Also, print the RST
     * string 'header' as section header.
     */
    private static void printCategory(
            FileWriter file, List<Rule> rules, String categoryName, String header)
            throws Exception {
        final String title = categoryName + "-Related Rules";
        file.write(rstHeading(title, '=') + "\n\n");
        file.write(rstIndex(title) + "\n\n");
        file.write(header + "\n\n\n");

        ListIterator<Rule> iter = rules.listIterator();
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
            String header)
            throws Exception {
        file.write(rstAnchor(subcategoryName.replace(" ", "_")) + "\n\n");
        file.write(rstHeading(subcategoryName, '-') + "\n\n");
        file.write(rstIndex(subcategoryName + "-related rules") + "\n\n");
        file.write(header + "\n\n\n");

        ListIterator<Rule> iter = rules.listIterator();
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
        final AnalysisContext context = AnalysisContext.create();

        if (verbose) System.out.println("Analysing rule files in directories: " + rulesDirs);

        // Get all lkql files from directories to analyse.
        List<File> ruleDirectoryFiles = new ArrayList<>();
        for (var dir : rulesDirs)
            ruleDirectoryFiles.addAll(
                    Arrays.asList(
                            dir.listFiles(f -> f.canRead() && f.getName().endsWith(".lkql"))));

        List<AnalysisUnit> units = new ArrayList<>();

        // Parse all rule files.
        for (var ruleFile : ruleDirectoryFiles) {
            final AnalysisUnit unit = context.getUnitFromFile(ruleFile.getPath());
            if (verbose) System.out.println(" * " + unit.getFileName());

            if (unit.getDiagnostics().length > 0) {
                System.err.println("Error while parsing \"" + unit.getFileName() + "\":");
                for (var diag : unit.getDiagnostics()) System.err.println(diag);
            } else units.add(unit);
        }

        // Create rules objects, only keep check/unit_check FunDecls. We need to
        // use Collectors.toList() here instead of a direct call to toList()
        // because we rely on the fact that the list is muttable for the
        // subsequent calls to printCategory/printSubcategory (mostly for
        // performace).
        List<Rule> rules = new ArrayList<>();
        rules =
                units.stream()
                        .map(u -> isCheck(u))
                        .filter(u -> u != null)
                        .map(u -> new Rule(u))
                        .collect(Collectors.toList());

        if (verbose) System.out.println("Found " + rules.size() + " rules for documentation.");

        // Sort the rules alphabetically before generating documentation.
        Collections.sort(rules);

        if (!outputDir.exists()) outputDir.mkdirs();

        // Generate the list of rules.
        FileWriter listOfRules = new FileWriter(outputDir + "/list_of_rules.rst");

        listOfRules.write(
                """
                .. _List_of_Rules:

                **************************
                Alphabetical List of Rules
                **************************

                This section contains an alphabetized list of all the predefined
                GNATcheck rules.

                """);
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

        FileWriter predefinedRules = new FileWriter(outputDir + "/predefined_rules.rst");

        predefinedRules.write(
                """
                .. _Predefined_Rules:

                ****************
                Predefined Rules
                ****************

                .. index:: Predefined Rules

                The description of the rules currently implemented in ``gnatcheck`` is
                given in this chapter.
                The rule identifier is used as a parameter of ``gnatcheck``'s ``+R`` or ``-R``
                switches.

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
                         My_Rule: {Str: \"i_am_a_string\"} # If the rule param is named 'Str'
                      }

                   You can specify it through the ``+R`` option also by passing a string right
                   after the rule name:

                   .. code-block:: ada

                      +RMy_Rule:i_am_a_string  -- 'My_Rule' string param is set to "i_am_a_string"

                *list[string]*
                   The parameter value is a list of string.
                   In a LKQL rule options file, you can use the LKQL list type to specify the
                   parameter value:

                   .. code-block:: lkql

                      val rules = @{
                         My_Rule: {Lst: [\"One\", \"Two\", \"Three\"]} # If the rule param is named 'Lst'
                      }

                   Through the ``+R`` option, you can specify it as a collection of string
                   parameters separated by commas:

                   .. code-block:: ada

                      +RMy_Rule:One,Two,Three  -- 'My_Rule' string list param is set to ["One", "Two", "Three"]



                """);

        printCategory(
                predefinedRules,
                rules,
                "Style",
                """
                The rules in this section may be used to enforce various feature usages
                consistent with good software engineering, for example
                as described in Ada 95 Quality and Style.
                """);

        printSubcategory(
                predefinedRules,
                rules,
                "Style",
                "Tasking",
                """
                The rules in this subsection may be used to enforce various
                feature usages related to concurrency.
                """);

        printSubcategory(
                predefinedRules,
                rules,
                "Style",
                "Object Orientation",
                """
                The rules in this subsection may be used to enforce various
                feature usages related to Object-Oriented Programming.
                """);

        printSubcategory(
                predefinedRules,
                rules,
                "Style",
                "Portability",
                """
                The rules in this subsection may be used to enforce various
                feature usages that support program portability.
                """);

        printSubcategory(
                predefinedRules,
                rules,
                "Style",
                "Program Structure",
                """
                The rules in this subsection may be used to enforce feature usages
                related to program structure.
                """);

        printSubcategory(
                predefinedRules,
                rules,
                "Style",
                "Programming Practice",
                """
                The rules in this subsection may be used to enforce feature usages that
                relate to program maintainability.
                """);

        printSubcategory(
                predefinedRules,
                rules,
                "Style",
                "Readability",
                """
                The rules described in this subsection may be used to enforce feature usages
                that contribute towards readability.
                """);

        printCategory(
                predefinedRules,
                rules,
                "Feature",
                """
                The rules in this section can be used to enforce specific
                usage patterns for a variety of language features.
                """);

        printCategory(
                predefinedRules,
                rules,
                "Metrics",
                """
                The rules in this section can be used to enforce compliance with
                specific code metrics, by checking that the metrics computed for a program
                lie within user-specifiable bounds.
                Depending on the metric, there may be a lower bound, an upper bound, or both.
                A construct is flagged if the value of the metric exceeds the upper bound
                or is less than the lower bound.

                The name of any metrics rule consists of the prefix ``Metrics_``
                followed by the name of the corresponding metric:
                ``Essential_Complexity``, ``Cyclomatic_Complexity``, or
                ``LSLOC``.
                (The 'LSLOC' acronym stands for 'Logical Source Lines Of Code'.)
                The meaning and the computed values of the metrics are
                the same as in *gnatmetric*.

                For the ``+R`` option, each metrics rule has a numeric parameter
                specifying the bound (integer or real, depending on a metric).

                *Example:* the rule

                ::

                  +RMetrics_Cyclomatic_Complexity : 7


                means that all bodies with cyclomatic complexity exceeding 7 will be flagged.

                To turn OFF the check for cyclomatic complexity metric,
                use the following option:

                ::

                  -RMetrics_Cyclomatic_Complexity
                """);

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
                """);

        predefinedRules.close();

        if (!rules.isEmpty()) {
            System.err.println("Error: " + rules.size() + " rules not documented!");

            for (var r : rules) System.out.println(r.toString());
        }

        return 0;
    }
}
