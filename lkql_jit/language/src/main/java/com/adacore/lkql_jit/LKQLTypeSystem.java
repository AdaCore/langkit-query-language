//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.values.*;
import com.adacore.lkql_jit.values.interfaces.Indexable;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Nullish;
import com.adacore.lkql_jit.values.interop.LKQLList;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.adacore.lkql_jit.values.lists.LKQLArrayList;
import com.adacore.lkql_jit.values.sets.LKQLOrderedHashSet;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeCast;
import com.oracle.truffle.api.dsl.TypeCheck;
import com.oracle.truffle.api.dsl.TypeSystem;
import java.math.BigInteger;

/** The LKQL type system formalization. */
@TypeSystem(
    {
        LKQLUnit.class,
        boolean.class,
        long.class,
        BigInteger.class,
        String.class,
        LKQLPattern.class,
        LKQLFunction.class,
        LKQLProperty.class,
        LKQLTuple.class,
        LKQLArrayList.class,
        LKQLOrderedHashSet.class,
        LKQLStream.class,
        LKQLList.class,
        Indexable.class,
        Iterable.class,
        LangkitSupport.NodeInterface.class,
        LangkitSupport.TokenInterface.class,
        LangkitSupport.AnalysisUnit.class,
        Libadalang.RewritingContext.class,
        LangkitSupport.RewritingNodeInterface.class,
        LangkitSupport.MemberReferenceInterface.class,
        DynamicAdaNode.class,
        LKQLNamespace.class,
        LKQLObject.class,
        Nullish.class,
        LKQLRecValue.class,
    }
)
public abstract class LKQLTypeSystem {

    // ----- Unit value methods -----

    /** Get whether a value is a unit instance. */
    @TypeCheck(LKQLUnit.class)
    public static boolean isUnit(final Object value) {
        return value == LKQLUnit.INSTANCE;
    }

    /** Cast a value to the unit instance. */
    @TypeCast(LKQLUnit.class)
    public static LKQLUnit asUnit(@SuppressWarnings("unused") final Object value) {
        return LKQLUnit.INSTANCE;
    }

    // ----- Nullish values -----

    /** Get whether a value is nullish. */
    @TypeCheck(Nullish.class)
    public static boolean isNullish(final Object value) {
        return value == LKQLUnit.INSTANCE || value == LKQLNull.INSTANCE;
    }

    // ----- Integer value methods -----

    /** Cast a long value to a big integer. */
    @ImplicitCast
    @CompilerDirectives.TruffleBoundary
    public static BigInteger longToBigInteger(long value) {
        return BigInteger.valueOf(value);
    }
}
