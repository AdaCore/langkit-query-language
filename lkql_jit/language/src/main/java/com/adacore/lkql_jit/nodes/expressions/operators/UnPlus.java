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
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;

import java.math.BigInteger;


/**
 * This node represents the arithmetic unary "plus" operation in the LKQL language (i.e. +5)
 *
 * @author Hugo GUERRIER
 */
public abstract class UnPlus extends UnOp {

    // ----- Constructors -----

    /**
     * Create an arithmetic unary "plus" node
     *
     * @param location    The location of the node in the source
     * @param argLocation The location of the argument node
     */
    protected UnPlus(
        SourceLocation location,
        DummyLocation argLocation
    ) {
        super(location, argLocation);
    }

    // ----- Execution methods -----

    /**
     * Return the positivated long value
     *
     * @param arg The value to positivate
     * @return The positivated value
     */
    @Specialization
    protected long getLong(long arg) {
        return arg;
    }

    /**
     * Return the positivated big integer value
     *
     * @param arg The value to positivate
     * @return The positivated value
     */
    @Specialization
    protected BigInteger getBigInteger(BigInteger arg) {
        return arg;
    }

    /**
     * Fallback error method when the argument is a non-integer value
     *
     * @param arg The non-integer argument
     */
    @Fallback
    protected void notNumber(Object arg) {
        throw LKQLRuntimeException.wrongType(
            LKQLTypesHelper.LKQL_INTEGER,
            LKQLTypesHelper.fromJava(arg),
            this.argLocation
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
