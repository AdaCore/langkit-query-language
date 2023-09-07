/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.LKQLRootNode;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.parser.ASTTranslator;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFactory;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Option;
import com.oracle.truffle.api.TruffleLanguage;
import org.graalvm.options.OptionCategory;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.options.OptionStability;

import java.io.File;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;


/**
 * This class represents the registration and the entry point of the LKQL language Truffle implementation
 *
 * @author Hugo GUERRIER
 */
@TruffleLanguage.Registration(
    id = "lkql",
    name = "Langkit Query Language",
    defaultMimeType = LKQLLanguage.MIME_TYPE,
    characterMimeTypes = LKQLLanguage.MIME_TYPE,
    contextPolicy = TruffleLanguage.ContextPolicy.EXCLUSIVE,
    dependentLanguages = {"regex"}
)
public final class LKQLLanguage extends TruffleLanguage<LKQLContext> {

    // ----- Macros and enum -----

    /**
     * The kind of rendering to use when emitting diagnostic.
     */
    public enum DiagnosticOutputMode {
        /**
         * Emit a pretty diagnostic with source listing where the diagnostic location is highlighted
         */
        PRETTY,
        /**
         * Use a GNATCheck-compliant format: "{file}:{line}:{col} check: {message} [{check}]"
         */
        GNATCHECK
    }

    /**
     * The MIME type of the language
     */
    public static final String MIME_TYPE = "application/langkit-query-language";

    /**
     * The reference to the LKQL language
     */
    private static final LanguageReference<LKQLLanguage> LANGUAGE_REFERENCE = LanguageReference.create(LKQLLanguage.class);

    /**
     * The reference to the LKQL context
     */
    private static final ContextReference<LKQLContext> CONTEXT_REFERENCE = ContextReference.create(LKQLLanguage.class);

    /**
     * If the current language spawning support the color
     */
    public static boolean SUPPORT_COLOR = false;

    // ----- Options -----


    // --- Language options

    /**
     * The LKQL JIT is in checker mode or not
     */
    @Option(
        help = "If the JIT is in checker mode",
        category = OptionCategory.INTERNAL,
        stability = OptionStability.STABLE
    )
    static final OptionKey<Boolean> checkerMode = new OptionKey<>(false);

    /**
     * The option to define if the language is verbose
     */
    @Option(
        help = "If the language should be verbose",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<Boolean> verbose = new OptionKey<>(false);


    // --- LKQL options

    /**
     * The option to define the charset of the LKQL sources
     */
    @Option(
        help = "The LKQL source charset",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> charset = new OptionKey<>("");

    /**
     * The option to define the project file to analyze
     */
    @Option(
        help = "The GPR project file to load",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> projectFile = new OptionKey<>("");

    /**
     * The name of the subproject to analyze. If empty, use the root project instead
     */
    @Option(
        help = "The name of the subproject to analyze.",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> subprojectFile = new OptionKey<>("");

    /**
     * The scenario variables to load the project file with, where "key=value" variable specifications
     * are encoded as Base64 and separated by semicolons.
     */
    @Option(
        help = "The scenario variables to load the project file with",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> scenarioVars = new OptionKey<>("");

    /**
     * Whether to create an auto provider with the specified files if no project is provided.
     */
    @Option(
        help = "Whether to create an auto provider with the specified files if no project" +
            "is provided.",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<Boolean> useAutoProvider = new OptionKey<>(false);

    /**
     * The option to define the files to analyze
     */
    @Option(
        help = "The ada files to analyze in LKQL",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> files = new OptionKey<>("");

    /**
     * The option to define the jobs
     */
    @Option(
        help = "The number of parallel jobs in the LKQL interpreter",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<Integer> jobs = new OptionKey<>(0);


    // --- Checker options

    /**
     * The option to define the checker debug mode
     */
    @Option(
        help = "If the checker is in debug mode",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<Boolean> checkerDebug = new OptionKey<>(false);

    /**
     * The option to define the directories to look the rules from
     */
    @Option(
        help = "The directories to search rules in",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> rulesDirs = new OptionKey<>("");

    /**
     * The option to specify the rule to run
     */
    @Option(
        help = "The comma separated rules to apply",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> rules = new OptionKey<>("");

    /**
     * The option to control what should be done when no rules are provided
     */
    @Option(
        help = "If true, consider that an empty value for 'rules' means to run all the rules",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<Boolean> fallbackToAllRules = new OptionKey<>(true);

    /**
     * The option to control what should be done when a source file cannot be found
     */
    @Option(
        help = "If true, do not stop the engine when a source file cannot be found",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<Boolean> keepGoingOnMissingFile = new OptionKey<>(false);

    /**
     * The option to specify arguments for the rules
     */
    @Option(
        help = "Arguments for the LKQL rules",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> rulesArgs = new OptionKey<>("");

    /**
     * The option to specify the files to ignore during the checking
     */
    @Option(
        help = "Files to ignore during the analysis",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> ignores = new OptionKey<>("");

    /**
     * The option to specify the error recovery mode
     */
    @Option(
        help = "The mode of error recovery in the checker",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> errorMode = new OptionKey<>("");

    @Option(
        help = "The message emitter",
        category = OptionCategory.USER,
        stability = OptionStability.STABLE
    )
    static final OptionKey<DiagnosticOutputMode> diagnosticOutputMode = new OptionKey<>(DiagnosticOutputMode.PRETTY);


    // ----- Attributes -----

    /**
     * The lkql analysis context from langkit
     */
    private final Liblkqllang.AnalysisContext analysisContext;

    // ----- Constructors -----

    /**
     * A simple constructor for the library loading
     */
    public LKQLLanguage() {
        super();

        // Create the langkit analysis context
        this.analysisContext = Liblkqllang.AnalysisContext.create();

        // Set the color support flag
        SUPPORT_COLOR = System.getenv("TERM") != null && System.console() != null;
    }

    // ----- Class methods -----

    /**
     * Get the context for a given node
     *
     * @param node The node to get the context from
     * @return The LKQLContext for the node
     */
    public static LKQLContext getContext(LKQLNode node) {
        return CONTEXT_REFERENCE.get(node);
    }

    /**
     * Get the language instance for the given node
     *
     * @param node The node to get the language instance from
     * @return The LKQLLanguage instance for the node
     */
    public static LKQLLanguage getLanguage(LKQLNode node) {
        return LANGUAGE_REFERENCE.get(node);
    }

    // ----- Language methods -----

    /**
     * @see com.oracle.truffle.api.TruffleLanguage#createContext(com.oracle.truffle.api.TruffleLanguage.Env)
     */
    @Override
    protected LKQLContext createContext(Env env) {
        // Get the built-in factory
        BuiltInFactory factory = BuiltInFactory.getInstance();

        // Create the global values
        GlobalScope globalValues = new GlobalScope(factory.getNbBuiltInFunctions());
        factory.addBuiltIns(globalValues);

        // Return the new context
        return new LKQLContext(env, globalValues);
    }

    /**
     * @see com.oracle.truffle.api.TruffleLanguage#initializeContext(Object)
     */
    @Override
    protected void initializeContext(LKQLContext context) {
        // Set output and error encoding to UTF_8, in the future, user should be able to choose
        System.setOut(new PrintStream(System.out, true, StandardCharsets.UTF_8));
        System.setErr(new PrintStream(System.err, true, StandardCharsets.UTF_8));

        // Initialize the source to analyse in the further execution
        context.initSources();
    }

    /**
     * @see com.oracle.truffle.api.TruffleLanguage#patchContext(Object, Env)
     */
    @Override
    protected boolean patchContext(LKQLContext context, Env newEnv) {
        context.patchContext(newEnv);
        return true;
    }

    /**
     * @see com.oracle.truffle.api.TruffleLanguage#finalizeContext(Object)
     */
    @Override
    protected void finalizeContext(LKQLContext context) {
        context.finalizeContext();
    }

    /**
     * @see com.oracle.truffle.api.TruffleLanguage#getOptionDescriptors()
     */
    @Override
    protected OptionDescriptors getOptionDescriptors() {
        return new LKQLLanguageOptionDescriptors();
    }

    /**
     * @see com.oracle.truffle.api.TruffleLanguage#parse(ParsingRequest)
     */
    @Override
    protected CallTarget parse(ParsingRequest request) {
        // Parse the given file or buffer
        TopLevelList topLevelList = this.getTopLevelList(request);

        // Get the LKQL context
        LKQLContext context = getContext(topLevelList);

        // If the checker mode is enabled, add the rule import to the top level node
        // And parse the rule arguments
        if (context.isChecker() && context.isRootContext()) {
            this.addRuleImports(topLevelList, context);
            this.parseRulesArgs(context);
        }

        // Return the call target
        return new LKQLRootNode(topLevelList, this).getCallTarget();
    }

    /**
     * Get a LKQL top level list from a parsing request
     *
     * @param request The parsing request
     * @return The LKQL top level list
     */
    private TopLevelList getTopLevelList(ParsingRequest request) {
        // Parse the given file or buffer
        Liblkqllang.AnalysisUnit unit;
        if (request.getSource().getPath() == null) {
            unit = this.analysisContext.getUnitFromBuffer(request.getSource().getCharacters().toString(), "<command-line>");
        } else {
            unit = this.analysisContext.getUnitFromFile(request.getSource().getPath());
        }

        // Verify the parsing result
        List<Liblkqllang.Diagnostic> diagnostics = unit.getDiagnostics();
        if (diagnostics.size() > 0) {
            throw LKQLRuntimeException.parsingException(diagnostics, request.getSource());
        }

        // Get the lkql program
        Liblkqllang.TopLevelList lktRootNode = (Liblkqllang.TopLevelList) unit.getRoot();

        // Translate the Langkit tree to a Truffle tree
        ASTTranslator translator = new ASTTranslator(request.getSource());
        TopLevelList lkqlProgram = (TopLevelList) lktRootNode.accept(translator);

        // Get the LKQL context
        LKQLContext context = getContext(lkqlProgram);

        // Make the verbose prints
        if (context.isVerbose()) {
//            System.out.println("=== Langkit AST <" + lkqlProgram.getLocation().getFileName() + "> :\n" +
//                    lktRootNode.dump() + '\n');
//            System.out.println("=== Truffle AST <" + lkqlProgram.getLocation().getFileName() + "> :\n" +
//                    lkqlProgram.toString(0) + '\n');
        }

        // Return the result
        return lkqlProgram;
    }

    /**
     * Add the rule import on the top level list
     *
     * @param topLevelList The top level list node to change
     * @param context      The LKQL context
     */
    private void addRuleImports(TopLevelList topLevelList, LKQLContext context) {
        // Get the rule dirs
        String[] ruleDirs = context.getRulesDirs();

        // For each rule dirs, explore it
        for (String ruleDirName : ruleDirs) {
            File ruleDir = new File(ruleDirName);
            if (ruleDir.isDirectory() && ruleDir.canRead()) {

                // Get all LKQL files in the rule dir
                File[] ruleFiles = ruleDir.listFiles(file -> file.getName().endsWith(".lkql"));
                if (ruleFiles != null) {
                    topLevelList.addRuleImports(
                        Arrays.stream(ruleFiles)
                            .filter(file -> file.isFile() && file.canRead())
                            .map(file -> file.getName().replace(".lkql", ""))
                            .toList()
                            .toArray(new String[0])
                    );
                }

            }
        }
    }

    /**
     * Parse the rules arguments and add them to the context
     *
     * @param context The context to place the arguments in
     */
    private void parseRulesArgs(LKQLContext context) {
        // Split the rules arguments
        String[] rulesArgsSources = context.getEnv().getOptions().get(LKQLLanguage.rulesArgs).split(";");

        for (String ruleArgSource : rulesArgsSources) {
            // Verify that the rule is not empty
            if (ruleArgSource.isEmpty() || ruleArgSource.isBlank()) continue;

            // Split the get the names and the value
            String[] valueSplit = ruleArgSource.split("=");
            String[] nameSplit = valueSplit[0].split("\\.");

            // Verify the rule argument syntax
            if (valueSplit.length != 2 || nameSplit.length != 2) {
                throw LKQLRuntimeException.fromMessage("Rule argument syntax error : '" + ruleArgSource + "'");
            }

            // Get the information from the rule argument source
            String ruleName = nameSplit[0].toLowerCase().trim();
            String argName = nameSplit[1].toLowerCase().trim();
            String valueSource = valueSplit[1].trim();

            // Execute the value source
            Liblkqllang.AnalysisUnit unit = this.analysisContext.getUnitFromBuffer(
                valueSource,
                "rule_argument",
                null,
                Liblkqllang.GrammarRule.EXPR_RULE
            );
            Liblkqllang.LkqlNode root = unit.getRoot();
            ASTTranslator translator = new ASTTranslator(null);
            LKQLNode node = root.accept(translator);
            try {
                Object value = node.executeGeneric(null);
                // Add the argument in the context
                context.addRuleArg(ruleName, argName, value);
            } catch (Exception e) {
                throw LKQLRuntimeException.fromMessage(
                    "The rule argument value generated an interpreter error: " + valueSource
                );
            }
        }
    }
}
