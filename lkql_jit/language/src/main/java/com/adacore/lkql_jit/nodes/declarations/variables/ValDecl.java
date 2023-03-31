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

package com.adacore.lkql_jit.nodes.declarations.variables;

import com.adacore.lkql_jit.nodes.declarations.DeclAnnotation;
import com.adacore.lkql_jit.nodes.declarations.Declaration;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;


/**
 * This class represents the base of a variable declaration in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class ValDecl extends Declaration {

    // ----- Attributes -----

    /**
     * The name of the variable
     */
    protected final String name;

    /**
     * The slot to put the variable in
     */
    protected final int slot;

    /**
     * The value of the variable
     */
    protected final Expr value;

    // ----- Constructors -----

    /**
     * Create a new variable declaration with the wanted parameters
     *
     * @param location   The location of the node in the source
     * @param annotation The annotation of the variable declaration
     * @param name       The name of the variable
     * @param slot       The slot to put the variable in
     * @param value      The value of the variable
     */
    protected ValDecl(
        SourceLocation location,
        DeclAnnotation annotation,
        String name,
        int slot,
        Expr value
    ) {
        super(location);
        this.annotation = annotation;
        this.name = name;
        this.slot = slot;
        this.value = value;
    }

}
