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
import com.adacore.libadalang.Libadalang;
import org.graalvm.word.WordFactory;


/**
 * This value represent the null value in the LKQL language, it extends the ada node class because null is a special
 * shape of node
 *
 * @author Hugo GUERRIER
 */
public final class NullValue extends Libadalang.AdaNode implements Nullish, Truthy {

    // ----- Attributes -----

    /** The unique instance of the null value in the LKQL language */
    private static NullValue instance = null;

    // ----- Constructors -----

    /**
     * Create a new null value, private for the singleton
     */
    private NullValue() {
        super(new Libadalang.Entity(
                Libadalang.CustomPointer.nullPointer(),
                new Libadalang.EntityInfo(
                        new Libadalang.Metadata(
                                false,
                                Libadalang.CustomPointer.nullPointer(),
                                Libadalang.CustomPointer.nullPointer()
                        ),
                        Libadalang.CustomPointer.nullPointer(),
                        false
                )
        ));
    }

    /**
     * Get the only instance of null value
     *
     * @return The instance of null value
     */
    public static NullValue getInstance() {
        if(instance == null) {
            instance = new NullValue();
        }
        return instance;
    }

    // ----- Value methods methods -----

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.Truthy#isTruthy() */
    @Override
    public boolean isTruthy() {
        return false;
    }

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue) */
    @Override
    public boolean internalEquals(LKQLValue o) {
        return o == this;
    }
    
    // ----- Override methods -----

    /** @see com.adacore.libadalang.Libadalang.AdaNode#isNull() */
    @Override
    public boolean isNull() {
        return true;
    }

    @Override
    public String toString() {
        return "null";
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this;
    }

    @Override
    public int hashCode() {
        return 0;
    }

}
