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

package com.adacore.lkql_jit.nodes.declarations.selector;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.LKQLDepthValue;
import com.adacore.lkql_jit.built_ins.values.LKQLRecValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This node represents an arm for a selector declaration in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorArm extends LKQLNode {

    // ----- Children -----

    /** The pattern to match. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    /** The expression to return if the arm is executed. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new selector arm.
     *
     * @param location The token location in the source.
     * @param pattern The pattern for the arm.
     * @param expr The expression to return.
     */
    public SelectorArm(SourceLocation location, BasePattern pattern, Expr expr) {
        super(location);
        this.pattern = pattern;
        this.expr = expr;
    }

    // ----- Getters -----

    public BasePattern getPattern() {
        return pattern;
    }

    public Expr getExpr() {
        return expr;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the selector arm and return if the node match.
     *
     * @param frame The frame to execute the arm in.
     * @param node The node to match.
     * @return The result of the arm execution or null if the arm doesn't match.
     */
    public LKQLRecValue executeArm(VirtualFrame frame, LKQLDepthValue node) {
        if (this.pattern.executeValue(frame, node.value)) {
            final var expr = this.expr.executeGeneric(frame);

            if (LKQLTypeSystemGen.isLKQLRecValue(expr)) {
                LKQLRecValue val = LKQLTypeSystemGen.asLKQLRecValue(expr);
                val.depth = node.depth + 1;
                return val;
            } else if (LKQLTypeSystemGen.isNullish(expr)) {
                return new LKQLRecValue(new Object[0], new Object[0]);
            } else {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_REC_VALUE, LKQLTypesHelper.fromJava(expr), this.expr);
            }
        }
        // Return null if the arm hasn't been executed
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
