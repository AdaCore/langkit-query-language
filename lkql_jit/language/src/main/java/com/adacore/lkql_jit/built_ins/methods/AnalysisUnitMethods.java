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

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInFunctionValue.create;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltinFunctionBody;
import com.adacore.lkql_jit.built_ins.values.LKQLNull;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.ArrayList;
import java.util.Map;

/**
 * This class contains all built-in methods for the analysis unit type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class AnalysisUnitMethods {

    public static final Map<String, BuiltInFunctionValue> methods =
            Map.ofEntries(
                    create(
                            "root",
                            "Return the root for this unit",
                            new String[] {"unit"},
                            new Expr[] {null},
                            new RootExpr()),
                    create(
                            "name",
                            "Return the name of this unit",
                            new String[] {"unit"},
                            new Expr[] {null},
                            new NameExpr()),
                    create(
                            "tokens",
                            "Return the tokens of the unit",
                            new String[] {"unit"},
                            new Expr[] {null},
                            new TokensExpr()),
                    create(
                            "text",
                            "Return the text of the analysis unit",
                            new String[] {"unit"},
                            new Expr[] {null},
                            new TextExpr()));

    // ----- Inner classes -----

    /** Expression of the "root" method. */
    public static final class RootExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.AdaNode res =
                    LKQLTypeSystemGen.asAnalysisUnit(frame.getArguments()[0]).getRoot();
            return res.isNone() ? LKQLNull.INSTANCE : res;
        }
    }

    /** Expression of the "name" method. */
    public static final class NameExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAnalysisUnit(frame.getArguments()[0]).getFileName();
        }
    }

    /** Expression of the "tokens" method. */
    public static final class TokensExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.AnalysisUnit unit =
                    LKQLTypeSystemGen.asAnalysisUnit(frame.getArguments()[0]);
            Libadalang.Token current = unit.getFirstToken();
            Libadalang.Token last = unit.getLastToken();
            ArrayList<Libadalang.Token> resList = new ArrayList<>();
            while (!current.isEquivalent(last) && !current.isNone()) {
                resList.add(current);
                current = current.next();
            }
            return new LKQLList(resList.toArray(new Libadalang.Token[0]));
        }
    }

    /** Expression of the "text" method. */
    public static final class TextExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAnalysisUnit(frame.getArguments()[0]).getText();
        }
    }
}
