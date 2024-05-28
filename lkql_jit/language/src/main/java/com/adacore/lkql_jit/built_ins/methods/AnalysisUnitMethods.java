//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.create;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
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

    public static final Map<String, BuiltInMethodFactory> methods =
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
    public static final class RootExpr extends BuiltInBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.AdaNode res =
                    LKQLTypeSystemGen.asAnalysisUnit(frame.getArguments()[0]).getRoot();
            return res.isNone() ? LKQLNull.INSTANCE : res;
        }
    }

    /** Expression of the "name" method. */
    public static final class NameExpr extends BuiltInBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAnalysisUnit(frame.getArguments()[0]).getFileName();
        }
    }

    /** Expression of the "tokens" method. */
    public static final class TokensExpr extends BuiltInBody {
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
    public static final class TextExpr extends BuiltInBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAnalysisUnit(frame.getArguments()[0]).getText();
        }
    }
}
