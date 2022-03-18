/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.runtime.values.*;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_functions.BigIntegerUtils;
import com.adacore.lkql_jit.utils.util_functions.ObjectUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.adacore.libadalang.Libadalang;

import java.math.BigInteger;


/**
 * This node represents the equality verification in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class BinEq extends BinOp {

    // ----- Constructors -----

    /**
     * Create an equality verification node
     *
     * @param location The location of the node in the source
     * @param leftLocation The location of the left node
     * @param rightLocation The location of the right node
     */
    protected BinEq(
            SourceLocation location,
            DummyLocation leftLocation,
            DummyLocation rightLocation
    ) {
        super(location, leftLocation, rightLocation);
    }

    // ----- Execution methods -----

    /**
     * Do the equality verification on unit values, always true
     *
     * @param left The left unit value
     * @param right The right unit value
     * @return True, unit is always equals to unit
     */
    @Specialization
    protected boolean eqUnit(@SuppressWarnings("unused") UnitValue left, @SuppressWarnings("unused") UnitValue right) {
        return true;
    }

    /**
     * Do the equality verification on longs
     *
     * @param left The left long value
     * @param right The right long value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqLongs(long left, long right) {
        return left == right;
    }

    /**
     * Do the equality verification on big integers
     *
     * @param left The left big integer value
     * @param right The right big integer value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqBigIntegers(BigInteger left, BigInteger right) {
        return BigIntegerUtils.equals(left, right);
    }

    /**
     * Do the equality verification on strings
     *
     * @param left The left string value
     * @param right The right string value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqStrings(String left, String right) {
        return left.equals(right);
    }

    /**
     * Do the equality verification on patterns
     *
     * @param left The left pattern value
     * @param right The right pattern value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqPatterns(Pattern left, Pattern right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on functions
     *
     * @param left The left function value
     * @param right The right function value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqFunctions(FunctionValue left, FunctionValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on property references
     *
     * @param left The left property reference value
     * @param right The right property reference value
     * @return The result of the equality verification
     */
    protected boolean eqPropertyRefs(PropertyRefValue left, PropertyRefValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on selectors
     *
     * @param left The left selector value
     * @param right The right selector value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqSelectors(SelectorValue left, SelectorValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on tuples
     *
     * @param left The left tuple value
     * @param right The right tuple value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqTuples(TupleValue left, TupleValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on lists
     *
     * @param left The left list value
     * @param right The right list value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqLists(ListValue left, ListValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on lazy lists
     *
     * @param left The left lazy list value
     * @param right The right lazy list value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqLazyLists(LazyListValue left, LazyListValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on selector lists
     *
     * @param left The left selector list value
     * @param right The right selector list value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqSelectorLists(SelectorListValue left, SelectorListValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on nodes
     *
     * @param left The left node value
     * @param right The right node value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqNodes(Libadalang.AdaNode left, Libadalang.AdaNode right) {
        return left.equals(right);
    }

    /**
     * Do the equality verification on tokens
     *
     * @param left The left token value
     * @param right The right token value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqTokens(Libadalang.Token left, Libadalang.Token right) {
        return left.equals(right);
    }

    /**
     * Do the equality verification on booleans
     *
     * @param left The left boolean value
     * @param right The right boolean value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqBooleans(boolean left, boolean right) {
        return left == right;
    }

    /**
     * Do the equality verification on objects
     *
     * @param left The left object value
     * @param right The right object value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqObjects(ObjectValue left, ObjectValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on namespace
     *
     * @param left The left namespace value
     * @param right The right namespace value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqNamespaces(NamespaceValue left, NamespaceValue right) {
        return left.internalEquals(right);
    }

    /**
     * Do the equality verification on analysis units
     *
     * @param left The left analysis unit value
     * @param right The right analysis unit value
     * @return The result of the equality verification
     */
    @Specialization
    protected boolean eqAnalysisUnits(Libadalang.AnalysisUnit left, Libadalang.AnalysisUnit right) {
        return left.equals(right);
    }

    /**
     * Do the equality verification on not comparable values
     *
     * @param left The left value
     * @param right The right value
     * @return Always false because it's not comparable
     */
    @Fallback
    protected boolean eqNotComparable(@SuppressWarnings("unused") Object left, @SuppressWarnings("unused") Object right) {
        return false;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
