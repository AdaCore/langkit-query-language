//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang.Token;
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
        public long onToken(Token t) {
            return t.sourceLocationRange.start.column;
        }
    }

    @BuiltInMethod(name = "end_column", doc = "Return the end column", isProperty = true)
    abstract static class EndColExpr extends BuiltInBody {

        @Specialization
        public long onToken(Token t) {
            return t.sourceLocationRange.end.column;
        }
    }

    @BuiltInMethod(name = "start_line", doc = "Return the start line", isProperty = true)
    abstract static class StartLineExpr extends BuiltInBody {

        @Specialization
        public long onToken(Token t) {
            return t.sourceLocationRange.start.line;
        }
    }

    @BuiltInMethod(name = "end_line", doc = "Return the end line", isProperty = true)
    abstract static class EndLineExpr extends BuiltInBody {

        @Specialization
        public long onToken(Token t) {
            return t.sourceLocationRange.end.line;
        }
    }

    @BuiltInMethod(
        name = "is_equivalent",
        doc = "Return whether two tokens are structurally equivalent"
    )
    abstract static class IsEquivalentExpr extends BuiltInBody {

        @Specialization
        protected boolean onValid(Token self, Token other) {
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
        public boolean onToken(Token t) {
            return t.triviaIndex != 0;
        }
    }

    @BuiltInMethod(name = "next", doc = "Return the next token")
    abstract static class NextExpr extends BuiltInBody {

        @Specialization
        protected Token onValid(Token receiver, @DefaultVal("false") boolean ignoreTrivia) {
            // Skip trivia if required
            Token res = receiver.next();
            if (ignoreTrivia) {
                while (!res.isNone() && res.triviaIndex != 0) {
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
        protected Token onValid(Token receiver, @DefaultVal("false") boolean excludeTrivia) {
            // Skip trivia if required
            Token res = receiver.previous();
            if (excludeTrivia) {
                while (!res.isNone() && res.triviaIndex != 0) {
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
        public LangkitSupport.AnalysisUnit onToken(Token t) {
            return t.unit;
        }
    }

    @BuiltInMethod(name = "text", doc = "Return the text for this token", isProperty = true)
    abstract static class TextExpr extends BuiltInBody {

        @Specialization
        public String onToken(Token t) {
            return t.getText();
        }
    }

    @BuiltInMethod(name = "kind", doc = "Return the kind for this token", isProperty = true)
    abstract static class KindExpr extends BuiltInBody {

        @Specialization
        public String onToken(Token t) {
            if (t.kind.toC() == -1) return "no_token";
            String rawKind = ObjectUtils.toString(t.kind);
            return StringUtils.toLowerCase(StringUtils.split(rawKind, "_")[1]);
        }
    }
}
