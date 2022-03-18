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

package com.adacore.lkql_jit.runtime.values.interfaces;


/**
 * This interface defines the LKQL values basic interface
 *
 * @author Hugo GUERRIER
 */
public interface LKQLValue {

    /**
     * Get the documentation for the LKQL value
     *
     * @return A string representing the LKQL documentation
     */
    default String getDocumentation() {
        return "";
    }

    /**
     * Get the profile for the callable LKQL value
     *
     * @return The string representing the callable profile
     */
    default String getProfile() {
        return "TODO : Implement the profile information";
    }

    /**
     * Do an internal equality verification
     *
     * @param o The other lkql value
     * @return True of the values are LKQLy equals, false else
     */
    boolean internalEquals(LKQLValue o);

}
