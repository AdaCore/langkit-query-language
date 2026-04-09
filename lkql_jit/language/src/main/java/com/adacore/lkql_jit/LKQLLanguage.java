//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import static com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper.createSection;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.liblktlang.Liblktlang;
import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.exceptions.LKQLStaticErrors;
import com.adacore.lkql_jit.langkit_translator.passes.FramingPass;
import com.adacore.lkql_jit.langkit_translator.passes.LktPasses;
import com.adacore.lkql_jit.langkit_translator.passes.ResolutionPass;
import com.adacore.lkql_jit.langkit_translator.passes.TranslationPass;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFrames;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.nodes.root_nodes.TopLevelRootNode;
import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.values.LKQLNamespace;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Option;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.source.Source;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;
import org.graalvm.options.OptionCategory;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.options.OptionStability;

/**
 * This class represents the registration and the entry point of the LKQL language Truffle
 * implementation.
 *
 * @author Hugo GUERRIER
 */
@TruffleLanguage.Registration(
    id = Constants.LKQL_ID,
    name = "Langkit Query Language",
    defaultMimeType = Constants.LKQL_MIME,
    characterMimeTypes = Constants.LKQL_MIME,
    contextPolicy = TruffleLanguage.ContextPolicy.EXCLUSIVE,
    dependentLanguages = { "regex" }
)
public final class LKQLLanguage extends TruffleLanguage<LKQLContext> {

    /**
     * This is the LKQL prelude. Those definitions are visible at the root of the LKQL context. This
     * is where we put all global definitions that must be accessible in every context
     */
    /*
     * TODO: Genericize LKQL issue #499. Cannot genericize the prelude because NODE_DESCRIPTION_MAP
     * doesn't contain any node named NodeInterface but AdaNode here for Ada.
     */
    private static final String PRELUDE_SOURCE = """
        selector children
        |" Yields all the descendants of the given node
        | AdaNode => rec(*this.children)
        | * => ()

        selector next_siblings
        |" Yields all the next siblings of the given node
        | AdaNode => rec(this.next_sibling())
        | * => ()

        selector parent
        |" Yields all the enclosing parents of the given node
        | AdaNode => rec(this.parent)
        | * => ()

        selector prev_siblings
        |" Yields all the previous siblings of the given node
        | AdaNode => rec(this.previous_sibling())
        | * => ()

        val all_nodes = units().flat_map((unit) => children(unit.root))
        """;

    // ----- Static variables -----

    /** The reference to the LKQL language. */
    private static final LanguageReference<LKQLLanguage> LANGUAGE_REFERENCE =
        LanguageReference.create(LKQLLanguage.class);

    /** The reference to the LKQL context. */
    private static final ContextReference<LKQLContext> CONTEXT_REFERENCE = ContextReference.create(
        LKQLLanguage.class
    );

    /** Whether the current language spawning support the color. */
    public static boolean SUPPORT_COLOR = false;

    // ----- Options -----

    /** The JSON encoded LKQL engine options. */
    @Option(
        help = "Options for the LKQL engine as a JSON object",
        category = OptionCategory.INTERNAL,
        stability = OptionStability.STABLE
    )
    static final OptionKey<String> options = new OptionKey<>("");

    Liblkqllang.AnalysisContext lkqlAnalysisContext;
    Liblktlang.AnalysisContext lktAnalysisContext;

    // ----- Constructors -----

    /** A simple constructor for the library loading. */
    public LKQLLanguage() {
        super();
        // We create an LKQL analysis context with a tab-stop size of 1 since Truffle.Source
        // columns counting is based on characters:
        // https://www.graalvm.org/truffle/javadoc/com/oracle/truffle/api/source/Source.html#createSection(int,int,int,int)
        this.lkqlAnalysisContext = Liblkqllang.AnalysisContext.create(
            (String) null,
            (Liblkqllang.FileReader) null,
            null,
            null,
            true,
            1
        );

        this.lktAnalysisContext = Liblktlang.AnalysisContext.create(
            null,
            null,
            null,
            null,
            true,
            1
        );
        // Set the color support flag
        SUPPORT_COLOR = System.getenv("TERM") != null && System.console() != null;
    }

    // ----- Class methods -----

    /**
     * Get the context for a given node.
     *
     * @param node The node to get the context from.
     * @return The LKQLContext for the node.
     */
    public static LKQLContext getContext(LKQLNode node) {
        return CONTEXT_REFERENCE.get(node);
    }

    /**
     * Get the language instance for the given node.
     *
     * @param node The node to get the language instance from.
     * @return The LKQLLanguage instance for the node.
     */
    public static LKQLLanguage getLanguage(LKQLNode node) {
        return LANGUAGE_REFERENCE.get(node);
    }

    // ----- Language methods -----

    /**
     * @see
     *     com.oracle.truffle.api.TruffleLanguage#createContext(com.oracle.truffle.api.TruffleLanguage.Env)
     */
    @Override
    protected LKQLContext createContext(Env env) {
        // Return the new context
        return new LKQLContext(env, new GlobalScope(), this);
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

    private void loadPrelude() {
        final var unit = lkqlAnalysisContext.getUnitFromBuffer(PRELUDE_SOURCE, "<prelude>");

        final var source = Source.newBuilder(
            Constants.LKQL_ID,
            unit.getText(),
            unit.getFileName()
        ).build();

        final var errors = new LKQLStaticErrors();
        final var truffleTree = lowerLKQL(source, unit.getRoot(), errors);
        if (!errors.diagnostics.isEmpty()) throw errors;

        final var namespace = (LKQLNamespace) new TopLevelRootNode(true, truffleTree, this)
            .getCallTarget()
            .call();
        getContext(truffleTree).getGlobal().loadPreludeNamespace(namespace);
    }

    @Override
    protected CallTarget parse(ParsingRequest request) {
        // Ensure the prelude is loaded first
        if (getContext(null).getGlobal().prelude == null) {
            loadPrelude();
        }

        // Translate the LKQL AST from Langkit to a Truffle AST
        final var result = translateSource(request.getSource());

        // If the current parsing request is the root request
        if (!request.getSource().isInternal()) {
            // Initialize the context source chain with the current source.
            getContext(result).fromStack.add(request.getSource());

            // And add rule imports to the TopLevelList if we're in a mode that requires it
            var engineMode = getContext(null).getEngineMode();
            if (engineMode == LKQLOptions.EngineMode.CHECKER) {
                result.addRuleImports();
            }
        }

        // Print the Truffle AST if the JIT is in debug mode
        if (getContext(result).isVerbose()) {
            System.out.println(
                "=== Truffle AST <" +
                    result.getSourceSection().getSource().getPath() +
                    "> :\n" +
                    result
            );
        }

        // Return the call target
        return new TopLevelRootNode(request.getSource().isInternal(), result, this).getCallTarget();
    }

    /**
     * Translate the given source Langkit AST. The source can be either legacy LKQL syntax (LKQL V1)
     * or Lkt syntax (future LKQL v2).
     *
     * <p>The default is LKQL syntax, but either syntaxes can be triggered by a comment in the first
     * line of the file: # lkql version: 1/2
     *
     * <p>The trigger is a simple string match, so the comment needs to match exactly that. We might
     * relax those constraints at a later stage.
     *
     * @param source The Truffle source of the AST.
     * @return The translated LKQL Truffle AST.
     */
    public TopLevelList translateSource(final Source source) {
        final var sourceType = SourceType.of(source);

        final var langkitCtx = switch (sourceType) {
            case LKQL -> lkqlAnalysisContext;
            case LKT -> lktAnalysisContext;
        };

        // Create a static error collector
        final var errors = new LKQLStaticErrors();

        // Then get the analysis unit from the provided source
        final var unit = getUnit(source, langkitCtx);

        // Iterate over diagnostics
        for (var diagnostic : unit.getDiagnostics()) {
            errors.addDiag(
                diagnostic.getMessage().getContent(),
                createSection(diagnostic.getSourceLocationRange(), source)
            );
        }
        // If parsing errors occurred throw here
        if (!errors.diagnostics.isEmpty()) throw errors;

        // Lower to Truffle nodes, according to the source type
        final TopLevelList truffleTree = switch (sourceType) {
            case LKQL -> lowerLKQL(source, (Liblkqllang.LkqlNode) unit.getRoot(), errors);
            case LKT -> lowerLkt(source, (Liblktlang.LangkitRoot) unit.getRoot(), errors);
        };
        // If some errors occurred during the translation, throw here
        if (!errors.diagnostics.isEmpty()) throw errors;

        final var resolutionPass = new ResolutionPass(errors);
        resolutionPass.passEntry(truffleTree);
        // If some errors occurred during the resolution pass, throw here
        if (!errors.diagnostics.isEmpty()) throw errors;

        // Finally return the Truffle tree ready for execution
        return truffleTree;
    }

    /** Shortcut to translate the given source from string. */
    public TopLevelList translateBuffer(String buffer, String bufferName) {
        Source src = Source.newBuilder(Constants.LKQL_ID, buffer, bufferName).build();
        return translateSource(src);
    }

    /** Get a unit from a source, using a buffer if needed, a file otherwise. */
    private LangkitSupport.AnalysisUnit getUnit(
        final Source source,
        final LangkitSupport.AnalysisContextInterface langkitCtx
    ) {
        if (source.getPath() == null) {
            return langkitCtx.getUnitFromBuffer(
                source.getCharacters().toString(),
                source.getName()
            );
        } else {
            return langkitCtx.getUnitFromFile(source.getPath());
        }
    }

    private TopLevelList lowerLKQL(
        final Source source,
        Liblkqllang.LkqlNode lkqlRoot,
        LKQLStaticErrors errors
    ) {
        // Do the framing pass to create the script frame descriptions
        final FramingPass framingPass = new FramingPass(source, errors);
        lkqlRoot.accept(framingPass);
        final ScriptFrames scriptFrames = framingPass
            .getScriptFramesBuilder()
            .build(CONTEXT_REFERENCE.get(null).getGlobal());

        // Do the translation pass and return the result
        final TranslationPass translationPass = new TranslationPass(source, scriptFrames, errors);
        return (TopLevelList) lkqlRoot.accept(translationPass);
    }

    private TopLevelList lowerLkt(
        final Source source,
        Liblktlang.LangkitRoot lktRoot,
        LKQLStaticErrors errors
    ) {
        // Create frames for the Lkt script
        final ScriptFrames frames = LktPasses.Frames.buildFrames(lktRoot).build(
            CONTEXT_REFERENCE.get(null).getGlobal()
        );

        // Then translate the Lkt parsing tree to a Truffle tree
        return LktPasses.buildLKQLNode(source, lktRoot, frames, errors);
    }

    // ----- Inner classes -----

    private enum SourceType {
        LKQL,
        LKT;

        /** Try to infer source type from the first line of a source. */
        static SourceType of(final Source source) {
            final var firstLine = source.getCharacters(1).toString();

            // No pragma, go with the default
            if (!firstLine.startsWith("# lkql version:")) return LKQL;

            final var matcher = Pattern.compile("# lkql version: ([0-9]+)").matcher(firstLine);

            // Not a valid int
            if (!matcher.matches()) throw LKQLEngineException.create("Invalid LKQL version");

            // If the first source line specify a correct LKQL version, branch accordingly.
            final var version = Integer.parseInt(matcher.group(1));
            switch (version) {
                case 1 -> {
                    return LKQL;
                }
                case 2 -> {
                    return LKT;
                }
                default -> throw LKQLEngineException.create("Invalid LKQL version");
            }
        }
    }
}
