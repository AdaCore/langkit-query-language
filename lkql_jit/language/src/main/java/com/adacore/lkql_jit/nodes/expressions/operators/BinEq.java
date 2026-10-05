//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.adacore.lkql_jit.values.*;
import com.adacore.lkql_jit.values.interop.LKQLDynamicObject;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.adacore.lkql_jit.values.lists.LKQLArrayList;
import com.adacore.lkql_jit.values.sets.LKQLOrderedHashSet;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;

/** This node represents an equality operation. */
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

    /** Do the equality verification on unit values, always true. */
    @Specialization
    protected boolean eqUnit(
        @SuppressWarnings("unused") final LKQLUnit left,
        @SuppressWarnings("unused") final LKQLUnit right
    ) {
        return true;
    }

    /** Do the equality verification on longs. */
    @Specialization
    protected boolean eqLongs(long left, long right) {
        return left == right;
    }

    /** Do the equality verification on big integers. */
    @Specialization
    protected boolean eqBigIntegers(BigInteger left, BigInteger right) {
        return BigIntegerUtils.equals(left, right);
    }

    /** Do the equality verification on strings. */
    @Specialization
    protected boolean eqStrings(String left, String right) {
        return left.equals(right);
    }

    /** Do the equality verification on patterns. */
    @Specialization
    protected boolean eqPatterns(LKQLPattern left, LKQLPattern right) {
        return left.equals(right);
    }

    /** Do the equality verification on functions. */
    @Specialization
    protected boolean eqFunctions(LKQLFunction left, LKQLFunction right) {
        return left.equals(right);
    }

    /** Do the equality verification on property references. */
    @Specialization
    protected boolean eqPropertyRefs(LKQLProperty left, LKQLProperty right) {
        return left.equals(right);
    }

    /** Do the equality verification on tuples. */
    @Specialization
    protected boolean eqTuples(LKQLTuple left, LKQLTuple right) {
        return left.equals(right);
    }

    /** Do the equality verification on lists. */
    @Specialization
    protected boolean eqLists(LKQLArrayList left, LKQLArrayList right) {
        return left.equals(right);
    }

    /** Do the equality verification on streams. */
    @Specialization
    protected boolean eqStreams(final LKQLStream left, final LKQLStream right) {
        return left == right;
    }

    /** Do the equality verification on sets. */
    @Specialization
    @CompilerDirectives.TruffleBoundary
    protected boolean eqSets(final LKQLOrderedHashSet left, final LKQLOrderedHashSet right) {
        return left.internalSet.equals(right.internalSet);
    }

    /** Do the equality verification on nodes. */
    @Specialization
    @CompilerDirectives.TruffleBoundary
    protected boolean eqNodes(
        LangkitSupport.NodeInterface left,
        LangkitSupport.NodeInterface right
    ) {
        return left.equals(right);
    }

    /** Do the equality verification on tokens. */
    @Specialization
    protected boolean eqTokens(
        LangkitSupport.TokenInterface left,
        LangkitSupport.TokenInterface right
    ) {
        return left.equals(right);
    }

    /** Do the equality verification on booleans. */
    @Specialization
    protected boolean eqBooleans(boolean left, boolean right) {
        return left == right;
    }

    /** Do the equality verification on objects. */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqObjects(
        LKQLObject left,
        LKQLObject right,
        @CachedLibrary("left") DynamicObjectLibrary leftLib,
        @CachedLibrary("right") DynamicObjectLibrary rightLib
    ) {
        return LKQLDynamicObject.compareWithLib(left, right, leftLib, rightLib);
    }

    /** Do the equality verification on namespace. */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected boolean eqNamespaces(
        LKQLNamespace left,
        LKQLNamespace right,
        @CachedLibrary("left") DynamicObjectLibrary leftLib,
        @CachedLibrary("right") DynamicObjectLibrary rightLib
    ) {
        return LKQLDynamicObject.compareWithLib(left, right, leftLib, rightLib);
    }

    /** Do the equality verification on analysis units. */
    @Specialization
    protected boolean eqAnalysisUnits(
        LangkitSupport.AnalysisUnit left,
        LangkitSupport.AnalysisUnit right
    ) {
        return left.equals(right);
    }

    /** Do the equality verification on not comparable values. */
    @Fallback
    protected boolean eqNotComparable(
        @SuppressWarnings("unused") Object left,
        @SuppressWarnings("unused") Object right
    ) {
        return false;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
