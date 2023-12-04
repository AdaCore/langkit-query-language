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

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.functions.*;
import java.util.HashMap;
import java.util.Map;

/**
 * This class contains the common built-in methods for all type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class CommonMethods implements BuiltInMethods {

    // ----- Attributes -----

    /** The methods map from their names to their function values. */
    protected final Map<String, BuiltInFunctionValue> methods;

    // ----- Constructors -----

    /** Create the common methods for all type. */
    protected CommonMethods() {
        this.methods = new HashMap<>();
        this.initMethods();
    }

    /** Initialize the common methods. */
    protected void initMethods() {
        this.methods.put(ImgFunction.NAME, ImgFunction.getValue());
        this.methods.put(PrintFunction.NAME, PrintFunction.getValue());
        this.methods.put(DocFunction.NAME, DocFunction.getValue());
        this.methods.put(ProfileFunction.NAME, ProfileFunction.getValue());
        this.methods.put(HelpFunction.NAME, HelpFunction.getValue());
    }

    // ----- Override methods -----

    /**
     * @see BuiltInMethods#getMethods()
     */
    @Override
    public Map<String, BuiltInFunctionValue> getMethods() {
        return this.methods;
    }
}
