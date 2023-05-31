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
     * @param source          The Truffle source of the AST.
     * @return The translated LKQL Truffle AST.
     */
    public static LKQLNode translate(
        final Liblkqllang.LkqlNode lkqlLangkitRoot,
        final Source source
    ) {
        // Do the framing pass to create the script frame descriptions
        final FramingPass framingPass = new FramingPass(source);
        lkqlLangkitRoot.accept(framingPass);
        final ScriptFrames scriptFrames = framingPass.getScriptFramesBuilder().build();

        // Do the translation pass and return the result
        final TranslationPass translationPass = new TranslationPass(source, scriptFrames);
        return lkqlLangkitRoot.accept(translationPass);
    }

}
