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

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;

/**
 * This node represents an expression detail value in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class DetailExpr extends DetailValue {

    // ----- Children -----

    /** The expression of the detail value. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new detail expression node.
     *
     * @param location The location of the node in the source.
     * @param expr The expression of the detail value.
     */
    public DetailExpr(SourceLocation location, Expr expr) {
        super(location);
        this.expr = expr;
    }

    @Specialization(guards = {"other != null"})
    public boolean onLKQLValues(
            VirtualFrame frame,
            LKQLValue value,
            @Cached("getExprAsLKQLValue(frame)") LKQLValue other) {
        InteropLibrary valueLibrary = InteropLibrary.getUncached(value);
        InteropLibrary expectedLibrary = InteropLibrary.getUncached(other);
        return valueLibrary.isIdentical(value, other, expectedLibrary);
    }

    @Fallback
    public boolean onLKQLValues(
            VirtualFrame frame, Object value, @Cached("getExpr(frame)") Object other) {
        return ObjectUtils.equals(value, other);
    }

    public Object getExpr(VirtualFrame frame) {
        return this.expr.executeGeneric(frame);
    }

    public LKQLValue getExprAsLKQLValue(VirtualFrame frame) {
        var val = this.expr.executeGeneric(frame);
        if (LKQLTypeSystemGen.isLKQLValue(val)) {
            return LKQLTypeSystemGen.asLKQLValue(val);
        }
        return null;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
