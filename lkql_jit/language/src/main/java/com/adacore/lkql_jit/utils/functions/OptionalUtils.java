//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.Optional;
import java.util.function.Function;

/** This class contains all utils for Optional objects beyond Truffle boundaries. */
public class OptionalUtils {

    @CompilerDirectives.TruffleBoundary
    public static <T, U> Optional<U> map(Optional<T> opt, Function<? super T, ? extends U> f) {
        return opt.map(f);
    }
}
