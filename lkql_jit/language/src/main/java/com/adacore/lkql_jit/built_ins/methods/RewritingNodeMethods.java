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

import static com.adacore.lkql_jit.built_ins.BuiltInFunctionValue.create;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltinFunctionBody;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.Map;

/** This class contains all methods for the rewriting node type. */
public final class RewritingNodeMethods {

    public static final Map<String, BuiltInFunctionValue> methods =
            Map.ofEntries(
                    create(
                            "clone",
                            "Given a rewriting node, clone it and return its copy",
                            new String[] {"node"},
                            new Expr[] {null},
                            new CloneExpr()));

    /** Body for the clone method */
    public static final class CloneExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asRewritingNode(frame.getArguments()[0]).clone();
        }
    }
}
