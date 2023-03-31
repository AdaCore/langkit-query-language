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

package com.adacore.lkql_jit.runtime.built_ins.methods;

import com.adacore.lkql_jit.utils.LKQLTypesHelper;


/**
 * This class contains all built-in methods for the lazy list type in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class LazyListMethods extends IterableMethods {

    // ----- Attributes -----

    /**
     * The only instance of the method collection
     */
    private static LazyListMethods instance = null;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private LazyListMethods() {
        super();
    }

    /**
     * Get the only instance of the method collection
     *
     * @return The instance
     */
    public static LazyListMethods getInstance() {
        if (instance == null) {
            instance = new LazyListMethods();
        }
        return instance;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.methods.BuiltInMethods#getType()
     */
    @Override
    public String getType() {
        return LKQLTypesHelper.LKQL_LAZY_LIST;
    }

}
