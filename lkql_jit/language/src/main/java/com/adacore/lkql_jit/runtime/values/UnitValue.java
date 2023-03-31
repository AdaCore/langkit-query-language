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

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Nullish;
import com.adacore.lkql_jit.runtime.values.interfaces.Truthy;


/**
 * This class represents the unit value in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class UnitValue implements Nullish, Truthy {

    // ----- Attributes -----

    /**
     * The unique instance of the unit value in the language
     */
    private static UnitValue instance = null;

    // ----- Constructors -----

    /**
     * Create a new unit value, private for the singleton
     */
    private UnitValue() {
    }

    /**
     * Get the unique instance of the unit value
     *
     * @return The unit value
     */
    public static UnitValue getInstance() {
        if (instance == null) {
            instance = new UnitValue();
        }
        return instance;
    }

    // ----- Value methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.Truthy#isTruthy()
     */
    @Override
    public boolean isTruthy() {
        return false;
    }

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    public boolean internalEquals(LKQLValue o) {
        return o == this;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "()";
    }

}
