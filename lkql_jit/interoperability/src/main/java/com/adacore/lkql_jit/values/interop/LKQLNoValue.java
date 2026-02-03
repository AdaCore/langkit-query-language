//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import com.oracle.truffle.api.interop.TruffleObject;

/**
 * This instance MUST be used when calling LKQL function from outside the engine to signify that a
 * parameter has no provided value, and thus use its default value.
 * Do not provide Java `null` because the latter is wrapped in a `Polyglot.Value` and cannot be
 * compared to `null` once in the engine.
 * (see <a href="https://www.graalvm.org/sdk/javadoc/org/graalvm/polyglot/Context.html#asValue(java.lang.Object)">Polyglot.Context.asValue</a>)
 */
public final class LKQLNoValue implements TruffleObject {

    public static final LKQLNoValue INSTANCE = new LKQLNoValue();

    private LKQLNoValue() {}
}
