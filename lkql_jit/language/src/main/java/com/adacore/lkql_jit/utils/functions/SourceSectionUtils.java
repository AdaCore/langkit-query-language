//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.adacore.langkit_support.LangkitSupport;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

public class SourceSectionUtils {

    /**
     * Create a new Truffle source section from a Langkit source location range and a Truffle
     * source.
     */
    public static SourceSection createSection(
        LangkitSupport.SourceLocationRange sloc,
        Source source
    ) {
        int end_col = sloc.end.column - 1;
        if (sloc.start.line == sloc.end.line) {
            end_col = Math.max(sloc.end.column - 1, sloc.start.column);
        }
        return source.createSection(sloc.start.line, sloc.start.column, sloc.end.line, end_col);
    }
}
