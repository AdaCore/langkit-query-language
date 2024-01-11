/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins.functions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltinFunctionBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.functions.FileUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.TreeMap;

/**
 * This class represents the "apply_quick_fixes" LKQL built-in function. This is an internal
 * built-in, used by the checker to apply all collected rewriting actions.
 */
public final class ApplyQuickFixes {

    // ----- Attributes -----

    /** The name of the function. */
    public static final String NAME = "apply_quick_fixes";

    // ----- Class methods -----

    public static BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Apply all collected quick fix actions",
                new String[0],
                new Expr[0],
                new ApplyQuickFixesExpr());
    }

    // ----- Inner classes -----

    /** Expression representing the "apply_quick_fixes" execution. */
    private static final class ApplyQuickFixesExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // If there is a rewriting context, apply the fixes. Else it means that none of the
            // processed rules performed an auto-fix.
            final var context = LKQLLanguage.getContext(this);
            if (context.hasRewritingContext()) {
                this.applyRewritingUnits(context, context.getRewritingContext());
            }

            // Return the unit value
            return LKQLUnit.INSTANCE;
        }

        /** Function to apply the rewriting units of the given rewriting context. */
        @CompilerDirectives.TruffleBoundary
        private void applyRewritingUnits(
                LKQLContext context, Libadalang.RewritingContext rewritingContext) {
            // Iterate over rewriting units in the rewriting context, and add them to sorted map
            // to ensure a deterministic output order.
            final var patches = new TreeMap<String, Libadalang.RewritingUnit>();
            for (var unit : rewritingContext.rewritingUnits()) {
                patches.put(unit.getAnalysisUnit().getFileName(true), unit);
            }

            // For each rewriting unit, perform the required auto fix actions
            for (String patchedFileName : patches.keySet()) {
                final var unit = patches.get(patchedFileName);
                final var patchedUnitSource = unit.unparse();
                switch (context.getAutoFixMode()) {
                        // If the required action is the display, just print the patched unit
                    case DISPLAY:
                        var header =
                                StringUtils.concat(
                                        "Patched '",
                                        unit.getAnalysisUnit().getFileName(false),
                                        "':\n");
                        header = StringUtils.concat(header, "=".repeat(header.length() - 1), "\n");
                        context.println(header);
                        context.println(patchedUnitSource);
                        break;

                        // If the required action is create a new file, create a new file alongside
                        // the original file and dump the patched unit inside it.
                        // The new file name is formed as this: <original_file_name>.patched.
                    case NEW_FILE:
                        final var newFile =
                                FileUtils.create(StringUtils.concat(patchedFileName, ".patched"));
                        writeInFile(newFile, patchedUnitSource);
                        break;

                        // If the required action is to patch the existing file, just replace the
                        // content of the original file with the patched unit.
                    case PATCH_FILE:
                        final var originalFile = FileUtils.create(patchedFileName);
                        writeInFile(originalFile, patchedUnitSource);
                        break;
                }
            }
        }

        /** Util internal function to write in a file out of the Truffle bounds. */
        @CompilerDirectives.TruffleBoundary
        private void writeInFile(File file, String content) {
            try (final var writer = new FileWriter(file)) {
                file.createNewFile();
                writer.write(content);
            } catch (IOException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
        }
    }
}
