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

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This node represents a pattern with a filter in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class FilteredPattern extends BasePattern {

    // ----- Children -----

    /**
     * The pattern to filter
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private UnfilteredPattern pattern;

    /**
     * The predicate to do the filtering
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr predicate;

    // ----- Constructors -----

    /**
     * Create a new filtered pattern node
     *
     * @param location  The location of the node in the source
     * @param pattern   The pattern to filter
     * @param predicate The predicate expression
     */
    public FilteredPattern(
        SourceLocation location,
        UnfilteredPattern pattern,
        Expr predicate
    ) {
        super(location);
        this.pattern = pattern;
        this.predicate = predicate;
    }


    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.patterns.BasePattern#executeNode(com.oracle.truffle.api.frame.VirtualFrame, com.adacore.libadalang.Libadalang.AdaNode)
     */
    @Override
    public boolean executeNode(VirtualFrame frame, Libadalang.AdaNode node) {
        // If the pattern match, execute the predicate
        if (this.pattern.executeNode(frame, node)) {
            // Try to execute the predicate in a boolean
            try {
                return this.predicate.executeBoolean(frame);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.predicate
                );
            }
        }

        // Return the failure
        return false;
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
