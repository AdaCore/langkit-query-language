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

package com.adacore.lkql_jit.nodes.expressions.match;

import com.adacore.lkql_jit.nodes.patterns.RegexPattern;
import com.adacore.lkql_jit.nodes.patterns.UniversalPattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;


/**
 * This node represents an arm from a match expression in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class MatchArm extends LKQLNode {

    // ----- Children -----

    /** The pattern to match during the execution */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    /** The result of the arm execution */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    // ----- Constructors -----

    /**
     * Create a new match arm node
     *
     * @param location The location of the node in the source
     * @param pattern The pattern of the match arm
     * @param expr The result of the match arm
     */
    protected MatchArm(
            SourceLocation location,
            BasePattern pattern,
            Expr expr
    ) {
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
    
    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Verify the pattern of the arm and if matched, execute the arm expression
     *
     * @param toMatch The node to match
     * @return The result of the arm expression if the pattern is valid, null else
     */
    public abstract Object executeArm(VirtualFrame frame, Object toMatch);

    /**
     * Execute the matching arm on a node value
     *
     * @param frame The frame to execute in
     * @param node The node value
     * @return The match expression if the pattern is valid
     */
    @Specialization
    protected Object onNode(VirtualFrame frame, Libadalang.AdaNode node) {
        if(this.pattern.executeNode(frame, node)) {
            return this.expr.executeGeneric(frame);
        } else {
            return null;
        }
    }

    /**
     * Execute the matching arm on a string value
     *
     * @param frame The frame to execute in
     * @param str The string value
     * @return The match expression if the pattern is valid
     */
    @Specialization
    protected Object onString(VirtualFrame frame, String str) {
        if(this.pattern.executeString(frame, str)) {
            return this.expr.executeGeneric(frame);
        } else {
            return null;
        }
    }

    /**
     * Fallback when the value cannot be matched
     *
     * @param frame The frame to execute in
     * @param other The value
     * @return Just null
     */
    @Fallback
    protected Object onOther(VirtualFrame frame, Object other) {
        return null;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
