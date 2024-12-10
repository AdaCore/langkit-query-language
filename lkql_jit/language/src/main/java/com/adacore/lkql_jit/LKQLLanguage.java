//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.langkit_translator.passes.FramingPass;
import com.adacore.lkql_jit.langkit_translator.passes.TranslationPass;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFrames;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.nodes.root_nodes.TopLevelRootNode;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.runtime.values.LKQLNamespace;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Option;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.source.Source;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
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
        dependentLanguages = {"regex"})
public final class LKQLLanguage extends TruffleLanguage<LKQLContext> {

    /**
     * This is the LKQL prelude. Those definitions are visible at the root of the LKQL context. This
     * is where we put all global definitions that must be accessible in every context
     */
    private static final String PRELUDE_SOURCE =
            """
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
        """;

    // ----- Static variables -----

    /** The reference to the LKQL language. */
    private static final LanguageReference<LKQLLanguage> LANGUAGE_REFERENCE =
            LanguageReference.create(LKQLLanguage.class);

    /** The reference to the LKQL context. */
    private static final ContextReference<LKQLContext> CONTEXT_REFERENCE =
            ContextReference.create(LKQLLanguage.class);

    /** Whether the current language spawning support the color. */
    public static boolean SUPPORT_COLOR = false;

    // ----- Options -----

    /** The JSON encoded LKQL engine options. */
    @Option(
            help = "Options for the LKQL engine as a JSON object",
            category = OptionCategory.INTERNAL,
            stability = OptionStability.STABLE)
    static final OptionKey<String> options = new OptionKey<>("");

    Liblkqllang.AnalysisContext lkqlAnalysisContext;

    // ----- Constructors -----

    /** A simple constructor for the library loading. */
    public LKQLLanguage() {
        super();
        this.lkqlAnalysisContext = Liblkqllang.AnalysisContext.create();
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
        // Create the global values
        GlobalScope globalValues = new GlobalScope();

        // Return the new context
        return new LKQLContext(env, globalValues, this);
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

    @Override
    protected CallTarget parse(ParsingRequest request) {
        final Liblkqllang.AnalysisUnit unit;
        TopLevelList result;

        if (request.getSource().getPath() == null) {
            unit =
                    lkqlAnalysisContext.getUnitFromBuffer(
                            request.getSource().getCharacters().toString(), "<command-line>");
        } else {
            unit = lkqlAnalysisContext.getUnitFromFile(request.getSource().getPath());
        }

        // Verify the parsing result
        final var diagnostics = unit.getDiagnostics();
        if (diagnostics.length > 0) {
            var ctx = LKQLLanguage.getContext(null);

            // Iterate over diagnostics
            for (Liblkqllang.Diagnostic diagnostic : diagnostics) {
                ctx.getDiagnosticEmitter()
                        .emitDiagnostic(
                                CheckerUtils.MessageKind.ERROR,
                                diagnostic.message.toString(),
                                null,
                                SourceSectionWrapper.create(
                                        diagnostic.sourceLocationRange, request.getSource()));
            }
            throw LKQLRuntimeException.fromMessage(
                    "Syntax errors in " + unit.getFileName(false) + ": stopping interpreter");
        }

        // Get the LKQL langkit AST
        final Liblkqllang.TopLevelList lkqlLangkitRoot = (Liblkqllang.TopLevelList) unit.getRoot();

        // Translate the LKQL AST from Langkit to a Truffle AST
        result = (TopLevelList) translate(lkqlLangkitRoot, request.getSource());

        // Print the Truffle AST if the JIT is in debug mode
        if (getContext(result).isVerbose()) {
            System.out.println(
                    "=== Truffle AST <"
                            + result.getSourceSection().getSource().getPath()
                            + "> :\n"
                            + result);
        }

        // Return the call target
        return new TopLevelRootNode(request.getSource().isInternal(), result, this).getCallTarget();
    }

    /**
     * Translate the given source Langkit AST.
     *
     * @param lkqlLangkitRoot The LKQL Langkit AST to translate.
     * @param source The Truffle source of the AST.
     * @return The translated LKQL Truffle AST.
     */
    public LKQLNode translate(
            final Liblkqllang.LkqlNode lkqlLangkitRoot, final Source source, boolean isPrelude) {

        if (!isPrelude) {
            var global = getContext(null).getGlobal();
            if (global.prelude == null) {
                // Eval prelude
                Source preludeSource =
                        Source.newBuilder(Constants.LKQL_ID, PRELUDE_SOURCE, "<prelude>").build();
                var root =
                        lkqlAnalysisContext
                                .getUnitFromBuffer(PRELUDE_SOURCE, "<prelude>")
                                .getRoot();
                var preludeRoot = (TopLevelList) translate(root, preludeSource, true);
                var callTarget = new TopLevelRootNode(true, preludeRoot, this).getCallTarget();
                global.prelude = (LKQLNamespace) callTarget.call();
                var preludeMap = global.prelude.asMap();

                var objects = new Object[preludeMap.size()];
                var i = 0;
                for (var entry : preludeMap.entrySet()) {
                    global.preludeMap.put(entry.getKey(), i);
                    objects[i] = entry.getValue();
                    i += 1;
                }
                global.preludeObjects = objects;
            }
        }

        // Do the framing pass to create the script frame descriptions
        final FramingPass framingPass = new FramingPass(source);
        lkqlLangkitRoot.accept(framingPass);
        final ScriptFrames scriptFrames =
                framingPass.getScriptFramesBuilder().build(CONTEXT_REFERENCE.get(null).getGlobal());

        // Do the translation pass and return the result
        final TranslationPass translationPass = new TranslationPass(source, scriptFrames);
        return lkqlLangkitRoot.accept(translationPass);
    }

    public LKQLNode translate(final Liblkqllang.LkqlNode lkqlLangkitRoot, final Source source) {
        return translate(lkqlLangkitRoot, source, false);
    }
}
