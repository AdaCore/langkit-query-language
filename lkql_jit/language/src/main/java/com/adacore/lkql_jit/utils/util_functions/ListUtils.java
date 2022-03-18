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

package com.adacore.lkql_jit.utils.util_functions;

import com.oracle.truffle.api.CompilerDirectives;

import java.util.List;


/**
 * Util functions for the java generic list type
 *
 * @author Hugo GUERRIER
 */
public final class ListUtils {

    /**
     * Get if a list contains an element
     *
     * @param list The list to verify
     * @param elem The element to find
     * @return True if the list contains the element
     * @param <T> The type of the element
     */
    @CompilerDirectives.TruffleBoundary
    public static <T> boolean contains(List<T> list, T elem) {
        return list.contains(elem);
    }

}
