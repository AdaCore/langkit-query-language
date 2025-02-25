//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.liblktlang.Liblktlang;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
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
import com.adacore.lkql_jit.runtime.values.LKQLNamespace;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Option;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.source.Source;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;
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

        // Translate the LKQL AST from Langkit to a Truffle AST
        result = (TopLevelList) translate(request.getSource());

        // If the current parsing request is the root request
        if (!request.getSource().isInternal()) {
            // Initialize the context source chain with the current source.
            getContext(result).fromStack.add(request.getSource());

            // And add rule imports to the TopLevelList if we're in a mode that requires it
            var engineMode = getContext(null).getEngineMode();
            if (
                engineMode == LKQLOptions.EngineMode.CHECKER ||
                engineMode == LKQLOptions.EngineMode.FIXER
            ) {
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
     * Private helper. Translate the given source Langkit AST to LKQLNode hierarchy.
     *
     * <p>This method works on LKQL roots and Lkt roots, dispatching on the proper parser and
     * translation pass as needed.
     */
    private LKQLNode translate(
        final LangkitSupport.NodeInterface root,
        final Source source,
        boolean isPrelude
    ) {
        if (!isPrelude) {
            var global = getContext(null).getGlobal();
            if (global.prelude == null) {
                // Eval prelude
                Source preludeSource = Source.newBuilder(
                    Constants.LKQL_ID,
                    PRELUDE_SOURCE,
                    "<prelude>"
                ).build();
                var preludeRoot = lkqlAnalysisContext
                    .getUnitFromBuffer(PRELUDE_SOURCE, "<prelude>")
                    .getRoot();
                var lkqlPrelude = (TopLevelList) translate(preludeRoot, preludeSource, true);
                var callTarget = new TopLevelRootNode(true, lkqlPrelude, this).getCallTarget();
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

        if (root instanceof Liblkqllang.LkqlNode lkqlRoot) {
            // Do the framing pass to create the script frame descriptions
            final FramingPass framingPass = new FramingPass(source);
            lkqlRoot.accept(framingPass);
            final ScriptFrames scriptFrames = framingPass
                .getScriptFramesBuilder()
                .build(CONTEXT_REFERENCE.get(null).getGlobal());

            // Do the translation pass and return the result
            final TranslationPass translationPass = new TranslationPass(source, scriptFrames);

            final var ast = lkqlRoot.accept(translationPass);
            if (!isPrelude) {
                final var resolutionPass = new ResolutionPass();
                resolutionPass.passEntry(ast);
            }
            return ast;
        } else if (root instanceof Liblktlang.LangkitRoot lktRoot) {
            final ScriptFrames frames = LktPasses.Frames.buildFrames(lktRoot).build(
                CONTEXT_REFERENCE.get(null).getGlobal()
            );

            final var ast = LktPasses.buildLKQLNode(source, lktRoot, frames);
            if (!isPrelude) {
                final var resolutionPass = new ResolutionPass();
                resolutionPass.passEntry(ast);
            }
            return ast;
        } else {
            throw LKQLRuntimeException.fromMessage("Should not happen");
        }
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
    public LKQLNode translate(final Source source, String sourceName) {
        var firstLine = new Scanner(source.getReader()).nextLine();
        LangkitSupport.AnalysisContextInterface langkitCtx = null;
        var baseName = source.getName().replaceFirst("[.][^.]+$", "");
        Source src;

        if (getContext(null).getOptions().autoTranslateUnits().contains(baseName)) {
            System.out.println("Translating " + source.getName() + " to lkt");
            String newSrc = source.getCharacters().toString();
            var lines = newSrc.split("\n");

            if (firstLine.startsWith("# lkql version:")) {
                if (firstLine.equals("# lkql version: 1")) {
                    // Translate LKQL to Lkt (LKQL v2)
                    newSrc = String.join("\n", Arrays.stream(lines).skip(1).toList());
                } else {
                    throw LKQLRuntimeException.fromMessage(
                        "Invalid lkql version line for autoTranslateUnit unit"
                    );
                }
            }

            var lktSrc = LKQLToLkt.lkqlToLkt(sourceName, newSrc);

            if (getContext(null).isVerbose()) {
                System.out.println("Lkt source for " + sourceName);
                System.out.println("=============================");
                System.out.println(lktSrc);
                System.out.println();
            }

            // Build a new source
            src = Source.newBuilder(Constants.LKQL_ID, lktSrc, source.getName()).build();

            langkitCtx = lktAnalysisContext;
        } else if (firstLine.startsWith("# lkql version:")) {
            if (firstLine.equals("# lkql version: 1")) {
                // lkql V1 uses lkql syntax
                langkitCtx = lkqlAnalysisContext;
            } else if (firstLine.equals("# lkql version: 2")) {
                // lkql V2 uses Lkt syntax
                langkitCtx = lktAnalysisContext;
            } else {
                throw LKQLRuntimeException.fromMessage("Invalid lkql version");
            }
        } else {
            // By default, use lkql syntax
            langkitCtx = lkqlAnalysisContext;
        }

        LangkitSupport.AnalysisUnit unit;
        if (source.getPath() == null) {
            unit = langkitCtx.getUnitFromBuffer(source.getCharacters().toString(), sourceName);
        } else {
            unit = langkitCtx.getUnitFromFile(source.getPath());
        }

        final var diagnostics = unit.getDiagnostics();
        if (diagnostics.length > 0) {
            var ctx = LKQLLanguage.getContext(null);

            // Iterate over diagnostics
            for (var diagnostic : diagnostics) {
                ctx
                    .getDiagnosticEmitter()
                    .emitDiagnostic(
                        CheckerUtils.MessageKind.ERROR,
                        diagnostic.getMessage().toString(),
                        null,
                        SourceSectionWrapper.create(diagnostic.getSourceLocationRange(), source)
                    );
            }
            throw LKQLRuntimeException.fromMessage(
                "Syntax errors in " + unit.getFileName(false) + ": stopping interpreter"
            );
        }

        return translate(unit.getRoot(), source, false);
    }

    /**
     * Shortcut to translate a source. If it has no name, it will be given the name
     * "<command-line>".
     */
    public LKQLNode translate(final Source source) {
        return translate(source, "<command-line>");
    }

    /** Shortcut to translate the given source from string. */
    public LKQLNode translate(String source, String sourceName) {
        Source src = Source.newBuilder(Constants.LKQL_ID, source, sourceName).build();
        return translate(src, sourceName);
    }
}
