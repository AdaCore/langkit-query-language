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

package com.adacore.lkql_jit.runtime;


/**
 * This class represents a storing cell in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Cell {

    // ----- Attributes -----

    /**
     * The reference stored in the cell.
     */
    private Object ref;

    // ----- Constructors -----

    /**
     * Create a new empty cell.
     */
    public Cell() {
        this.ref = null;
    }

    /**
     * Create a new cell with its reference.
     *
     * @param obj The object to store in the cell.
     */
    public Cell(Object obj) {
        this.ref = obj;
    }

    // ----- Getters -----

    public boolean isNull() {
        return this.ref == null;
    }

    public Object getRef() {
        return this.ref;
    }

    // ----- Setters -----

    public void clear() {
        this.ref = null;
    }

    public void setRef(Object ref) {
        this.ref = ref;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.isNull() ? "null" : this.ref.toString();
    }

}
