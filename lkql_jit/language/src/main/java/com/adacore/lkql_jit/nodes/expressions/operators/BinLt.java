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

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.BigIntegerUtils;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

import java.math.BigInteger;


/**
 * This node represents the "lower than" operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinLt extends BinOp {

    // ----- Constructors -----

    /**
     * Create a "lower than" node.
     *
     * @param location      The location of the node in the source.
     * @param leftLocation  The location of the left node.
     * @param rightLocation The location of the right node.
     */
    protected BinLt(
        SourceLocation location,
        DummyLocation leftLocation,
        DummyLocation rightLocation
    ) {
        super(location, leftLocation, rightLocation);
    }

    // ----- Execution methods -----

    /**
     * Do the "lower than" comparison on longs.
     *
     * @param left  The left long value.
     * @param right The right long value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean ltLongs(long left, long right) {
        return left < right;
    }

    /**
     * Do the "lower than" comparison on big integers.
     *
     * @param left  The left big integer value.
     * @param right The right big integer value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean ltBigInteger(BigInteger left, BigInteger right) {
        return BigIntegerUtils.compareTo(left, right) < 0;
    }

    /**
     * Do the "lower than" comparison on strings.
     *
     * @param left  The left string value.
     * @param right The right string value.
     * @return The result of the comparison.
     */
    @Specialization
    protected boolean ltString(String left, String right) {
        return left.compareTo(right) < 0;
    }

    /**
     * Do the "lower than" comparison on not comparable values.
     *
     * @param left  The left value.
     * @param right The right value.
     */
    @Fallback
    protected void notComparable(Object left, Object right) {
        throw LKQLRuntimeException.unsupportedOperation(
            LKQLTypesHelper.fromJava(left),
            "<",
            LKQLTypesHelper.fromJava(right),
            this
        );
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
