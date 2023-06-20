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

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.utils.source_location.Locatable;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;


/**
 * This class represents an identifier in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Identifier implements Locatable {

    // ----- Attributes -----

    /**
     * The location of the identifier in the source.
     */
    private final SourceLocation location;

    /**
     * The name of the identifier.
     */
    private final String name;

    // ----- Constructors -----

    /**
     * Create a new identifier with the parameters.
     *
     * @param location The location of the identifier in the source.
     * @param name     The name of the identifier.
     */
    public Identifier(
        SourceLocation location,
        String name
    ) {
        this.location = location;
        this.name = name;
    }

    // ----- Getters -----

    /**
     * @see com.adacore.lkql_jit.utils.source_location.Locatable#getLocation()
     */
    @Override
    public SourceLocation getLocation() {
        return this.location;
    }

    public String getName() {
        return this.name;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.name;
    }

}
