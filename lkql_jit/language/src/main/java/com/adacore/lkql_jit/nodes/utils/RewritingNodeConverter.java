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

package com.adacore.lkql_jit.nodes.utils;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.Locatable;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;

/** This node help for the conversion of a value to a rewriting node. */
public abstract class RewritingNodeConverter extends Node {

    /**
     * Convert the given value to a rewriting node, if this operation is possible. Else raise a
     * runtime error at the provided location.
     *
     * @param value The value to convert.
     * @param location The location of the conversion in the LKQL code.
     */
    public abstract Libadalang.RewritingNode execute(Object value, Locatable location);

    @Specialization
    protected Libadalang.RewritingNode onNull(LKQLNull nullValue, Locatable usage) {
        return Libadalang.RewritingNode.NONE;
    }

    @Specialization(guards = "!node.isNone()")
    protected Libadalang.RewritingNode onAdaNode(Libadalang.AdaNode node, Locatable usage) {
        return node.getRewritingNode().clone();
    }

    @Specialization
    protected Libadalang.RewritingNode onRewritingNode(
            Libadalang.RewritingNode node, Locatable usage) {
        return node;
    }

    @Fallback
    protected Libadalang.RewritingNode notNode(Object other, Locatable usage) {
        throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.typeUnion(LKQLTypesHelper.ADA_NODE, LKQLTypesHelper.REWRITING_NODE),
                LKQLTypesHelper.fromJava(other),
                usage);
    }
}
