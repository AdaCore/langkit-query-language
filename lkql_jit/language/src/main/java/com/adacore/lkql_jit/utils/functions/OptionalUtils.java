//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.Optional;
import java.util.function.Function;

/** Util functions to manipulate optional values with Truffle boundaries. */
public class OptionalUtils {

    /**
     * Map the provided optional value with the specified function, applying it to the value
     * contained in the optional if there is some.
     */
    @CompilerDirectives.TruffleBoundary
    public static <T, U> Optional<U> map(Optional<T> opt, Function<? super T, ? extends U> f) {
        return opt.map(f);
    }
}
