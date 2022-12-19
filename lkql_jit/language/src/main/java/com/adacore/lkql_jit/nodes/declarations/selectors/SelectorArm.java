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

package com.adacore.lkql_jit.nodes.declarations.selectors;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.values.DepthNode;

import java.util.ArrayList;
import java.util.List;


/**
 * This node represents an arm for a selector declaration in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class  SelectorArm extends LKQLNode {

    // ----- Children -----

    /** The pattern to match */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private BasePattern pattern;

    /** The expression to return if the arm is executed */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private SelectorExpr expr;

    // ----- Constructors -----

    /**
     * Create a new selector arm
     *
     * @param location The token location in the source
     * @param pattern The pattern for the arm
     * @param expr The expression to return
     */
    public SelectorArm(
            SourceLocation location,
            BasePattern pattern,
            SelectorExpr expr
    ) {
        super(location);
        this.pattern = pattern;
        this.expr = expr;
    }

    // ----- Getters -----

    public BasePattern getPattern() {
        return pattern;
    }

    public SelectorExpr getExpr() {
        return expr;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the selector arm and return if the node match
     *
     * @param frame The frame to execute the arm in
     * @param node The node to match
     * @return The result of the arm execution or null if the arm doesn't match
     */
    public SelectorRootNode.SelectorCallResult executeArm(VirtualFrame frame, DepthNode node) {
        if(this.pattern.executeNode(frame, node.getNode())) {
            // Execute the selector expression
            Object res = this.expr.executeGeneric(frame);

            // If the result of the expression is an array
            if(res instanceof Object[] resList) {
                List<DepthNode> depthNodes = new ArrayList<>(resList.length);
                for (Object obj : resList) {
                    // For each object of the array, verify that it is a node
                    try {
                        if (!LKQLTypeSystemGen.isNullish(obj)) {
                            depthNodes.add(new DepthNode(node.getDepth() + 1, LKQLTypeSystemGen.expectAdaNode(obj)));
                        }
                    }

                    // If it isn't a node, throw an exception
                    catch (UnexpectedResultException e) {
                        throw LKQLRuntimeException.wrongSelectorType(
                                LKQLTypesHelper.fromJava(obj),
                                this.expr
                        );
                    }
                }
                return new SelectorRootNode.SelectorCallResult(this.expr.getMode(), depthNodes.toArray(new DepthNode[0]));
            }

            // If the result of the expression is nullish
            else if(LKQLTypeSystemGen.isNullish(res)) {
                return new SelectorRootNode.SelectorCallResult(this.expr.getMode(), res);
            }

            // If the result of the expression is a node
            else if(LKQLTypeSystemGen.isAdaNode(res)) {
                return new SelectorRootNode.SelectorCallResult(
                        this.expr.getMode(),
                        new DepthNode(node.getDepth() + 1, LKQLTypeSystemGen.asAdaNode(res))
                );
            }

            // Throw an exception
            throw LKQLRuntimeException.wrongSelectorType(
                    LKQLTypesHelper.fromJava(res),
                    this.expr
            );
        }

        // Return null if the arm hasn't been executed
        return null;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
