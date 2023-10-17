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

package com.adacore.lkql_jit.nodes.patterns.chained_patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.util.ArrayList;
import java.util.List;

/**
 * This node is the base of all chained pattern links in LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class ChainedPatternLink extends LKQLNode {

    // ----- Children -----

    /** The pattern to match in the link. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected BasePattern pattern;

    // ----- Constructors -----

    /**
     * Create a new chained pattern link.
     *
     * @param location The location of the node in the source.
     * @param pattern The pattern to match.
     */
    protected ChainedPatternLink(SourceLocation location, BasePattern pattern) {
        super(location);
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the link with a node and return the list of the matched node.
     *
     * @param frame The frame to execute the link in.
     * @param node The input node in the chain.
     * @return The result of the chain link execution.
     */
    public abstract Libadalang.AdaNode[] executeLink(VirtualFrame frame, Libadalang.AdaNode node);

    // ----- Class methods -----

    /**
     * Do the pattern filtering logic on the given result object.
     *
     * @param frame The frame to execute the pattern in.
     * @param resultObject The result object to filter and add in the result.
     * @return The result of the filtering.
     */
    protected Libadalang.AdaNode[] doPatternFiltering(VirtualFrame frame, Object resultObject) {
        // Prepare the result list
        List<Libadalang.AdaNode> resList = new ArrayList<>();

        // If the result object is a node
        if (LKQLTypeSystemGen.isAdaNode(resultObject)) {
            if (this.pattern.executeNode(frame, LKQLTypeSystemGen.asAdaNode(resultObject))) {
                resList.add(LKQLTypeSystemGen.asAdaNode(resultObject));
            }
        }

        // If the result object is a list of node
        else if (LKQLTypeSystemGen.isListValue(resultObject)) {
            ListValue listValue = LKQLTypeSystemGen.asListValue(resultObject);
            for (int i = 0; i < listValue.size(); i++) {
                try {
                    Libadalang.AdaNode toVerify = LKQLTypeSystemGen.expectAdaNode(listValue.get(i));
                    if (this.pattern.executeNode(frame, toVerify)) {
                        resList.add(toVerify);
                    }
                } catch (UnexpectedResultException e) {
                    throw LKQLRuntimeException.wrongType(
                            LKQLTypesHelper.ADA_NODE,
                            LKQLTypesHelper.fromJava(e.getResult()),
                            this);
                }
            }
        }

        // Else, throw a type exception
        else {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_LIST, LKQLTypesHelper.fromJava(resultObject), this);
        }

        // Return the result as an array of node
        return resList.toArray(new Libadalang.AdaNode[0]);
    }
}
