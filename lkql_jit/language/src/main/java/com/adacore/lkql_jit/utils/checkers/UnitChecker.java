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

package com.adacore.lkql_jit.utils.checkers;

import com.adacore.lkql_jit.runtime.values.FunctionValue;

/** This class represents a unit checker in the LKQL system. */
public final class UnitChecker extends BaseChecker {

    // ----- Constructors -----

    /** Create a new unit checker. */
    public UnitChecker(
            final String name,
            final FunctionValue function,
            final String message,
            final String help,
            final boolean followGenericInstantiations,
            final String category,
            final String subcategory,
            final Remediation remediation,
            final long executionCost,
            final boolean parametricExemption,
            final String impact,
            final String target) {
        super(
                name,
                function,
                message,
                help,
                followGenericInstantiations,
                category,
                subcategory,
                remediation,
                executionCost,
                parametricExemption,
                impact,
                target);
    }

    // ----- Instance methods -----

    @Override
    public BaseChecker copy() {
        return new UnitChecker(
                this.name,
                this.function,
                this.message,
                this.help,
                this.followGenericInstantiations,
                this.category,
                this.subcategory,
                this.remediation,
                this.executionCost,
                this.parametricExemption,
                this.impact,
                this.target);
    }
}
