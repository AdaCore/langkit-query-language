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

package com.adacore.lkql_jit.nodes.expressions.value_read;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * A dynamic read from the global interactive scope. Such reads are not optimized by the compiler.
 * We might want to investigate optimizing them at some stage.
 */
public class ReadDynamic extends Expr {

    private final String name;

    public ReadDynamic(final SourceLocation location, String name) {
        super(location);
        this.name = name;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.getObject();
    }

    @CompilerDirectives.TruffleBoundary
    public Object getObject() {
        return LKQLLanguage.getContext(this).getGlobal().getGlobalObjects().get(this.name);
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"name"}, new Object[] {this.name});
    }
}
