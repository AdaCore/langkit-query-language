//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.libadalang.Libadalang;
import com.adacore.libadalang.Libadalang.AnalysisUnit;
import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Specialization;
import java.util.ArrayList;

/** This class contains all built-in methods for the analysis unit type in the LKQL language. */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.ANALYSIS_UNIT })
public final class AnalysisUnitMethods {

    @BuiltInMethod(name = "root", doc = "Return the root for this unit", isProperty = true)
    abstract static class RootExpr extends BuiltInBody {

        @Specialization
        public Object onUnit(AnalysisUnit self) {
            Libadalang.AdaNode res = self.getRoot();
            return res.isNone() ? LKQLNull.INSTANCE : res;
        }
    }

    @BuiltInMethod(name = "name", doc = "Return the name for this unit", isProperty = true)
    abstract static class NameExpr extends BuiltInBody {

        @Specialization
        public String onUnit(AnalysisUnit self) {
            return self.getFileName();
        }
    }

    @BuiltInMethod(name = "tokens", doc = "Return the tokens for this unit", isProperty = true)
    abstract static class TokensExpr extends BuiltInBody {

        @Specialization
        public LKQLList onUnit(AnalysisUnit self) {
            Libadalang.Token current = self.getFirstToken();
            Libadalang.Token last = self.getLastToken();
            ArrayList<Libadalang.Token> resList = new ArrayList<>();
            while (!current.isEquivalent(last) && !current.isNone()) {
                resList.add(current);
                current = current.next();
            }
            return new LKQLList(resList.toArray(new Libadalang.Token[0]));
        }
    }

    @BuiltInMethod(name = "text", doc = "Return the text for this unit", isProperty = true)
    abstract static class TextExpr extends BuiltInBody {

        @Specialization
        public String onUnit(AnalysisUnit self) {
            return self.getText();
        }
    }
}
