/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

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
