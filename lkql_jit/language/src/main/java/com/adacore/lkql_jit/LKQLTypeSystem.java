//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.values.*;
import com.adacore.lkql_jit.built_ins.values.interfaces.Iterable;
import com.adacore.lkql_jit.built_ins.values.interfaces.*;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLLazyList;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeCast;
import com.oracle.truffle.api.dsl.TypeCheck;
import com.oracle.truffle.api.dsl.TypeSystem;
import java.math.BigInteger;

/**
 * This class represents the type system of LKQL that contains type casts and checks.
 *
 * @author Hugo GUERRIER
 */
@TypeSystem({
    LKQLUnit.class,
    long.class,
    BigInteger.class,
    String.class,
    LKQLPattern.class,
    LKQLFunction.class,
    LKQLSelector.class,
    LKQLProperty.class,
    LKQLTuple.class,
    LKQLList.class,
    LKQLLazyList.class,
    Indexable.class,
    Iterable.class,
    Libadalang.AdaNode.class,
    Libadalang.Token.class,
    Libadalang.AnalysisUnit.class,
    boolean.class,
    LKQLNamespace.class,
    LKQLObject.class,
    Nullish.class,
    LKQLValue.class,
})
public abstract class LKQLTypeSystem {

    // ----- Unit value methods -----

    /**
     * Check if a value is unit.
     *
     * @param value The value to test.
     * @return True if the value is unit, false else.
     */
    @TypeCheck(LKQLUnit.class)
    public static boolean isUnit(final Object value) {
        return value == LKQLUnit.INSTANCE;
    }

    /**
     * Cast a value to unit.
     *
     * @param value The value to cast.
     * @return The unit value.
     */
    @TypeCast(LKQLUnit.class)
    public static LKQLUnit asUnit(@SuppressWarnings("unused") final Object value) {
        return LKQLUnit.INSTANCE;
    }

    // ----- Nullish values -----

    /**
     * Check if a value is nullish.
     *
     * @param value The value to check.
     * @return True if the value si nullish, false else.
     */
    @TypeCheck(Nullish.class)
    public static boolean isNullish(final Object value) {
        return value == LKQLUnit.INSTANCE || value == LKQLNull.INSTANCE;
    }

    // ----- Boolean value methods -----

    /**
     * Check if a value can be interpreted as a boolean.
     *
     * @param value The value to check.
     * @return True if the value can be a boolean, false else.
     */
    @TypeCheck(boolean.class)
    public static boolean isBoolean(Object value) {
        return value instanceof Boolean
                || value instanceof Truthy
                || value instanceof Libadalang.AdaNode;
    }

    /**
     * Cast a generic value to a boolean.
     *
     * @param value The value to case.
     * @return The boolean representation of the value.
     */
    @TypeCast(boolean.class)
    public static boolean asBoolean(Object value) {
        if (value instanceof Boolean bool) {
            return bool;
        } else if (value instanceof Truthy truthy) {
            return truthy.isTruthy();
        } else return value instanceof Libadalang.AdaNode;
    }

    // ----- Integer value methods -----

    /**
     * Cast a long value to a big integer value.
     *
     * @param value The long value to cast.
     * @return The value in big integer.
     */
    @ImplicitCast
    @CompilerDirectives.TruffleBoundary
    public static BigInteger longToBigInteger(long value) {
        return BigInteger.valueOf(value);
    }
}
