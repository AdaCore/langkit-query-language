//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.runtime.values.*;
import com.adacore.lkql_jit.runtime.values.lists.LKQLLazyList;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/**
 * This node represents the equality verification in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinEq extends BinOp {

    // ----- Constructors -----

    /**
     * Create an equality verification node.
     *
     * @param location The location of the node in the source.
     */
    protected BinEq(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * Do the equality verification on unit values, always true.
     *
     * @param left The left unit value.
     * @param right The right unit value.
     * @return True, unit is always equals to unit.
     */
    @Specialization
    protected boolean eqUnit(
        @SuppressWarnings("unused") final LKQLUnit left,
        @SuppressWarnings("unused") final LKQLUnit right
    ) {
        return true;
    }

    /**
     * Do the equality verification on longs.
     *
     * @param left The left long value.
     * @param right The right long value.
     * @return The result of the equality verification.
     */
    @Specialization
    protected boolean eqLongs(long left, long right) {
        return left == right;
    }

    /**
     * Do the equality verification on big integers.
     *
     * @param left The left big integer value.
     * @param right The right big integer value.
     * @return The result of the equality verification.
     */
    @Specialization
    protected boolean eqBigIntegers(BigInteger left, BigInteger right) {
        return BigIntegerUtils.equals(left, right);
    }

    /**
     * Do the equality verification on strings.
     *
     * @param left The left string value.
     * @param right The right string value.
     * @return The result of the equality verification.
     */
    @Specialization
    protected boolean eqStrings(String left, String right) {
        return left.equals(right);
    }

    /**
     * Do the equality verification on patterns.
     *
     * @param left The left pattern value.
     * @param right The right pattern value.
     * @return The result of the equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqPatterns(
        final LKQLPattern left,
        final LKQLPattern right,
        @CachedLibrary("left") InteropLibrary leftLibrary,
        @CachedLibrary("right") InteropLibrary rightLibrary
    ) {
        return leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the equality verification on functions.
     *
     * @param left The left function value.
     * @param right The right function value.
     * @return The result of the equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqFunctions(
        final LKQLFunction left,
        final LKQLFunction right,
        @CachedLibrary("left") InteropLibrary leftLibrary,
        @CachedLibrary("right") InteropLibrary rightLibrary
    ) {
        return leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the equality verification on property references.
     *
     * @param left The left property reference value.
     * @param right The right property reference value.
     * @return The result of the equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqPropertyRefs(
        final LKQLProperty left,
        final LKQLProperty right,
        @CachedLibrary("left") InteropLibrary leftLibrary,
        @CachedLibrary("right") InteropLibrary rightLibrary
    ) {
        return leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the equality verification on selectors.
     *
     * @param left The left selector value.
     * @param right The right selector value.
     * @return The result of the equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqSelectors(
        final LKQLSelector left,
        final LKQLSelector right,
        @CachedLibrary("left") InteropLibrary leftLibrary,
        @CachedLibrary("right") InteropLibrary rightLibrary
    ) {
        return leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the equality verification on tuples.
     *
     * @param left The left tuple value.
     * @param right The right tuple value.
     * @return The result of the equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqTuples(
        final LKQLTuple left,
        final LKQLTuple right,
        @CachedLibrary("left") InteropLibrary leftLibrary,
        @CachedLibrary("right") InteropLibrary rightLibrary
    ) {
        return leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the equality verification on lists.
     *
     * @param left The left list value.
     * @param right The right list value.
     * @return The result of the equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqLists(
        final LKQLList left,
        final LKQLList right,
        @CachedLibrary("left") InteropLibrary leftLibrary,
        @CachedLibrary("right") InteropLibrary rightLibrary
    ) {
        return leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /** Do the equality verification in lazy lists. */
    @Specialization
    protected boolean onLazyLists(final LKQLLazyList left, final LKQLLazyList right) {
        return left == right;
    }

    /**
     * Do the equality verification on nodes.
     *
     * @param left The left node value.
     * @param right The right node value.
     * @return The result of the equality verification.
     */
    @Specialization
    @CompilerDirectives.TruffleBoundary
    protected boolean eqNodes(
        LangkitSupport.NodeInterface left,
        LangkitSupport.NodeInterface right
    ) {
        return left.equals(right);
    }

    /**
     * Do the equality verification on tokens.
     *
     * @param left The left token value.
     * @param right The right token value.
     * @return The result of the equality verification.
     */
    @Specialization
    protected boolean eqTokens(
        LangkitSupport.TokenInterface left,
        LangkitSupport.TokenInterface right
    ) {
        return left.equals(right);
    }

    /**
     * Do the equality verification on booleans.
     *
     * @param left The left boolean value.
     * @param right The right boolean value.
     * @return The result of the equality verification.
     */
    @Specialization
    protected boolean eqBooleans(boolean left, boolean right) {
        return left == right;
    }

    /**
     * Do the equality verification on objects.
     *
     * @param left The left object value.
     * @param right The right object value.
     * @return The result of the equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqObjects(
        final LKQLObject left,
        final LKQLObject right,
        @CachedLibrary("left") InteropLibrary leftLibrary,
        @CachedLibrary("right") InteropLibrary rightLibrary
    ) {
        return leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the equality verification on namespace.
     *
     * @param left The left namespace value.
     * @param right The right namespace value.
     * @return The result of the equality verification.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqNamespaces(
        final LKQLNamespace left,
        final LKQLNamespace right,
        @CachedLibrary("left") InteropLibrary leftLibrary,
        @CachedLibrary("right") InteropLibrary rightLibrary
    ) {
        return leftLibrary.isIdentical(left, right, rightLibrary);
    }

    /**
     * Do the equality verification on analysis units.
     *
     * @param left The left analysis unit value.
     * @param right The right analysis unit value.
     * @return The result of the equality verification.
     */
    @Specialization
    protected boolean eqAnalysisUnits(
        LangkitSupport.AnalysisUnit left,
        LangkitSupport.AnalysisUnit right
    ) {
        return left.equals(right);
    }

    /**
     * Do the equality verification on not comparable values.
     *
     * @param left The left value.
     * @param right The right value.
     * @return Always false because it's not comparable.
     */
    @Fallback
    protected boolean eqNotComparable(
        @SuppressWarnings("unused") Object left,
        @SuppressWarnings("unused") Object right
    ) {
        return false;
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
