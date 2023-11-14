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

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.values.*;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLLazyList;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import java.math.BigInteger;

/**
 * This node represents the non-equality operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinNeq extends BinOp {

    // ----- Constructors -----

    /**
     * Create a non-equality node.
     *
     * @param location The location of the node in the source.
     * @param leftLocation The location of the left node.
     * @param rightLocation The location of the right node.
     */
    protected BinNeq(
            SourceLocation location, DummyLocation leftLocation, DummyLocation rightLocation) {
        super(location, leftLocation, rightLocation);
    }

    // ----- Execution methods -----

    /**
     * Do the non-equality verification on unit values, always false.
     *
     * @param left The left unit value.
     * @param right The right unit value.
     * @return False, two unit values are always equals.
     */
    @Specialization
    protected boolean neqUnit(
            @SuppressWarnings("unused") final LKQLUnit left,
            @SuppressWarnings("unused") final LKQLUnit right) {
        return false;
    }

    /**
     * Do the non-equality verification on longs.
     *
     * @param left The left long value.
     * @param right The right long value.
     * @return The result of the non-equality verification.
     */
    @Specialization
    protected boolean neqLongs(long left, long right) {
        return left != right;
    }

    /**
     * Do the non-equality verification on big integers.
     *
     * @param left The left big integer value.
     * @param right The right big integer value.
     * @return The result of the non-equality verification.
     */
    @Specialization
    protected boolean neqBigIntegers(BigInteger left, BigInteger right) {
        return !BigIntegerUtils.equals(left, right);
    }

    /**
     * Do the non-equality verification on strings.
     *
     * @param left The left string value.
     * @param right The right string value.
     * @return The result of the non-equality verification.
     */
    @Specialization
    protected boolean neqString(String left, String right) {
        return !left.equals(right);
    }

    /**
     * Do the non-equality verification on patterns.
     *
     * @param left The left pattern value.
     * @param right The right pattern value.
     * @return The result of the non-equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean neqPatterns(
            final LKQLPattern left,
            final LKQLPattern right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        return !leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the non-equality verification on functions.
     *
     * @param left The left function value.
     * @param right The right function value.
     * @return The result of the non-equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean neqFunctions(
            final LKQLFunction left,
            final LKQLFunction right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        return !leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the non-equality verification on property references.
     *
     * @param left The left property reference value.
     * @param right The right property reference value.
     * @return The result of the non-equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean neqPropertyRefs(
            final LKQLProperty left,
            final LKQLProperty right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        return !leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the non-equality verification on selectors.
     *
     * @param left The left selector value.
     * @param right The right selector value.
     * @return The result of the non-equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean neqSelectors(
            final LKQLSelector left,
            final LKQLSelector right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        return !leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the non-equality verification on tuples.
     *
     * @param left The left tuple value.
     * @param right The right tuple value.
     * @return The result of the non-equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean neqTuples(
            final LKQLTuple left,
            final LKQLTuple right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        return !leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the non-equality verification on lists.
     *
     * @param left The left list value.
     * @param right The right list value.
     * @return The result of the non-equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean neqLists(
            final LKQLList left,
            final LKQLList right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        return !leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /** Do the non-equality verification on lazy lists. */
    @Specialization
    protected boolean neqLazyLists(final LKQLLazyList left, final LKQLLazyList right) {
        return left != right;
    }

    /**
     * Do the non-equality verification on nodes.
     *
     * @param left The left node value.
     * @param right The right node value.
     * @return The result of the non-equality verification.
     */
    @Specialization
    protected boolean neqNodes(Libadalang.AdaNode left, Libadalang.AdaNode right) {
        return !left.equals(right);
    }

    /**
     * Do the non-equality verification on tokens.
     *
     * @param left The left token value.
     * @param right The right token value.
     * @return The result of the non-equality verification.
     */
    @Specialization
    protected boolean neqTokens(Libadalang.Token left, Libadalang.Token right) {
        return !left.equals(right);
    }

    /**
     * Do the non-equality verification on analysis units.
     *
     * @param left The left analysis unit value.
     * @param right The right analysis unit value.
     * @return The result of the non-equality verification.
     */
    @Specialization
    protected boolean neqAnalysisUnits(
            Libadalang.AnalysisUnit left, Libadalang.AnalysisUnit right) {
        return !left.equals(right);
    }

    /**
     * Do the non-equality verification on booleans.
     *
     * @param left The left boolean value.
     * @param right The right boolean value.
     * @return The result of the non-equality verification.
     */
    @Specialization
    protected boolean neqBooleans(boolean left, boolean right) {
        return left != right;
    }

    /**
     * Do the non-equality verification on objects.
     *
     * @param left The left object value.
     * @param right The right object value.
     * @return The result of the non-equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean neqObjects(
            final LKQLObject left,
            final LKQLObject right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        return !leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the non-equality verification on namespaces.
     *
     * @param left The left namespace value.
     * @param right The right namespace value
     * @return The result of the non-equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean neqNamespaces(
            final LKQLNamespace left,
            final LKQLNamespace right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        return !leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the non-equality verification on not comparable values.
     *
     * @param left The left value.
     * @param right The right value.
     * @return Always true because not comparable cannot be equals.
     */
    @Fallback
    protected boolean neqNotComparable(
            @SuppressWarnings("unused") Object left, @SuppressWarnings("unused") Object right) {
        return true;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
