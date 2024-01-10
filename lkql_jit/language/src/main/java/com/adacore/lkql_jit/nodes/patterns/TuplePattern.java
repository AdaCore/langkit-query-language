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

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.built_ins.values.LKQLTuple;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;

public abstract class TuplePattern extends ValuePattern {
    /** The sub-patterns for this tuple pattern. */
    @Node.Children private final BasePattern[] patterns;

    public TuplePattern(SourceLocation location, BasePattern[] patterns) {
        super(location);
        this.patterns = patterns;
    }

    @Specialization
    @ExplodeLoop
    public boolean onTuple(VirtualFrame frame, LKQLTuple tuple) {
        if (tuple.getArraySize() != patterns.length) {
            return false;
        }

        for (int i = 0; i < tuple.getArraySize(); i++) {
            if (!patterns[i].executeValue(frame, tuple.get(i))) {
                return false;
            }
        }
        return true;
    }

    @Fallback
    public boolean onOther(VirtualFrame frame, Object other) {
        return false;
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
