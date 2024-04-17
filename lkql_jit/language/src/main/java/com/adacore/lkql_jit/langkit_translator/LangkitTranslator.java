//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.langkit_translator.passes.FramingPass;
import com.adacore.lkql_jit.langkit_translator.passes.TranslationPass;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFrames;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.source.Source;

/**
 * This class is the top level class to translate a LKQL Langkit AST into a Truffle AST.
 *
 * @author Hugo GUERRIER
 */
public final class LangkitTranslator {

    /**
     * Translate the given source Langkit AST.
     *
     * @param lkqlLangkitRoot The LKQL Langkit AST to translate.
     * @param source The Truffle source of the AST.
     * @return The translated LKQL Truffle AST.
     */
    public static LKQLNode translate(
            final Liblkqllang.LkqlNode lkqlLangkitRoot, final Source source) {
        // Do the framing pass to create the script frame descriptions
        final FramingPass framingPass = new FramingPass(source);
        lkqlLangkitRoot.accept(framingPass);
        final ScriptFrames scriptFrames = framingPass.getScriptFramesBuilder().build();

        // Do the translation pass and return the result
        final TranslationPass translationPass = new TranslationPass(source, scriptFrames);
        return lkqlLangkitRoot.accept(translationPass);
    }
}
