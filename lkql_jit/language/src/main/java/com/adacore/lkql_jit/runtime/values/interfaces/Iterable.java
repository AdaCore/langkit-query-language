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

import com.adacore.lkql_jit.utils.util_classes.Iterator;


/**
 * This interface defines the iterable interface for all the iterable LKQL types
 *
 * @author Hugo GUERRIER
 */
public interface Iterable extends LKQLValue {

    /**
     * Get the size of the iterable collection
     *
     * @return The size of the iterable
     */
    long size();

    /**
     * Get if the iterable contains the given element
     *
     * @param elem The element to look for
     * @return True if the iterable contains the element, false else
     */
    boolean contains(Object elem);

    /**
     * Get the iterator for the iterable object
     *
     * @return The iterator
     */
    Iterator iterator();

}