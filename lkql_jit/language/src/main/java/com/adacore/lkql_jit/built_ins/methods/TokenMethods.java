//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.annotations.*;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Specialization;

/**
 * This class contains all built-in methods for the token type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.TOKEN })
public final class TokenMethods {

    @BuiltInMethod(name = "start_column", doc = "Return the start column", isProperty = true)
    abstract static class StartColExpr extends BuiltInBody {

        @Specialization
        public long onToken(LangkitSupport.TokenInterface t) {
            return t.getSourceLocationRange().start.column;
        }
    }

    @BuiltInMethod(name = "end_column", doc = "Return the end column", isProperty = true)
    abstract static class EndColExpr extends BuiltInBody {

        @Specialization
        public long onToken(LangkitSupport.TokenInterface t) {
            return t.getSourceLocationRange().end.column;
        }
    }

    @BuiltInMethod(name = "start_line", doc = "Return the start line", isProperty = true)
    abstract static class StartLineExpr extends BuiltInBody {

        @Specialization
        public long onToken(LangkitSupport.TokenInterface t) {
            return t.getSourceLocationRange().start.line;
        }
    }

    @BuiltInMethod(name = "end_line", doc = "Return the end line", isProperty = true)
    abstract static class EndLineExpr extends BuiltInBody {

        @Specialization
        public long onToken(LangkitSupport.TokenInterface t) {
            return t.getSourceLocationRange().end.line;
        }
    }

    @BuiltInMethod(
        name = "is_equivalent",
        doc = "Return whether two tokens are structurally equivalent"
    )
    abstract static class IsEquivalentExpr extends BuiltInBody {

        @Specialization
        protected boolean onValid(
            LangkitSupport.TokenInterface self,
            LangkitSupport.TokenInterface other
        ) {
            return self.isEquivalent(other);
        }
    }

    @BuiltInMethod(
        name = "is_trivia",
        doc = "Return whether this token is a trivia",
        isProperty = true
    )
    abstract static class IsTriviaExpr extends BuiltInBody {

        @Specialization
        public boolean onToken(LangkitSupport.TokenInterface t) {
            return t.isTrivia();
        }
    }

    @BuiltInMethod(name = "next", doc = "Return the next token")
    abstract static class NextExpr extends BuiltInBody {

        @Specialization
        protected LangkitSupport.TokenInterface onValid(
            LangkitSupport.TokenInterface receiver,
            @DefaultVal("false") boolean ignoreTrivia
        ) {
            // Skip trivia if required
            LangkitSupport.TokenInterface res = receiver.next();
            if (ignoreTrivia) {
                while (!res.isNone() && res.isTrivia()) {
                    res = res.next();
                }
            }

            // Return the result
            return res;
        }
    }

    @BuiltInMethod(name = "previous", doc = "Return the previous token")
    abstract static class PrevExpr extends BuiltInBody {

        @Specialization
        protected LangkitSupport.TokenInterface onValid(
            LangkitSupport.TokenInterface receiver,
            @DefaultVal("false") boolean excludeTrivia
        ) {
            // Skip trivia if required
            LangkitSupport.TokenInterface res = receiver.previous();
            if (excludeTrivia) {
                while (!res.isNone() && res.isTrivia()) {
                    res = res.previous();
                }
            }

            // Return the result
            return res;
        }
    }

    @BuiltInMethod(name = "unit", doc = "Return the unit for this token", isProperty = true)
    abstract static class UnitExpr extends BuiltInBody {

        @Specialization
        public LangkitSupport.AnalysisUnit onToken(LangkitSupport.TokenInterface t) {
            return t.getUnit();
        }
    }

    @BuiltInMethod(name = "text", doc = "Return the text for this token", isProperty = true)
    abstract static class TextExpr extends BuiltInBody {

        @Specialization
        public String onToken(LangkitSupport.TokenInterface t) {
            return t.getText();
        }
    }

    @BuiltInMethod(name = "kind", doc = "Return the kind for this token", isProperty = true)
    abstract static class KindExpr extends BuiltInBody {

        @Specialization
        public String onToken(LangkitSupport.TokenInterface t) {
            if (t.getKind().toC() == -1) return "no_token";
            String rawKind = ObjectUtils.toString(t.getKind());
            return StringUtils.toLowerCase(StringUtils.split(rawKind, "_")[1]);
        }
    }
}
