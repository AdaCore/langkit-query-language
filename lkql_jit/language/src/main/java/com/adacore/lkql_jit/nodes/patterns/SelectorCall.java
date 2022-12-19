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

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_classes.Iterator;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.runtime.values.DepthNode;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.runtime.values.SelectorListValue;
import com.adacore.lkql_jit.runtime.values.SelectorValue;

import java.util.ArrayList;
import java.util.List;


/**
 * This node represents the call of a selector in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class SelectorCall extends LKQLNode {

    // ----- Enums and macros -----

    /** Quantifiers for the selector call */
    public enum Quantifier {
        ANY,
        ALL
    }

    /** The modes of binding */
    public enum Mode {
        LOCAL,
        GLOBAL
    }

    // ----- Attributes -----

    /** The quantifier for the selector call */
    private final Quantifier quantifier;

    /** The slot to put the binding value in, might be -1 if there is no binding */
    private final int bindingSlot;

    /** The mode of the binding */
    private final Mode bindingMode;

    // ----- Children -----

    /** The selector to call */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr selectorExpr;

    /** The arguments for the selector call */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ArgList args;

    // ----- Constructors -----

    /**
     * Create a new selector call node
     *
     * @param location The location of the node in the source
     * @param quantifier The quantifier for the selector
     * @param bindingSlot The slot of the binding
     * @param bindingMode The mode of the binding
     * @param selectorExpr The selector expression
     * @param args The arguments for the call
     */
    public SelectorCall(
            SourceLocation location,
            Quantifier quantifier,
            int bindingSlot,
            Mode bindingMode,
            Expr selectorExpr,
            ArgList args
    ) {
        super(location);
        this.quantifier = quantifier;
        this.bindingSlot = bindingSlot;
        this.bindingMode = bindingMode;
        this.selectorExpr = selectorExpr;
        this.args = args;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the selector on the given node and return if the tree traversal valid the given pattern
     *
     * @param frame The frame to execute in
     * @param node The node to execute the selector on
     * @param pattern The pattern to verify
     * @return True if the traversal verify the pattern, false else
     */
    public boolean executeVerification(VirtualFrame frame, Libadalang.AdaNode node, BasePattern pattern) {
        // Get the selector list
        SelectorListValue selectorListValue = this.getSelectorList(frame, node);

        // Get the result of the validation
        boolean isValid;
        if(this.quantifier == Quantifier.ALL) {
            isValid = this.isAll(frame, selectorListValue, pattern);
        } else {
            isValid = this.isAny(frame, selectorListValue, pattern);
        }

        // Do the bindings
        if(this.bindingSlot > -1) {
            this.doBinding(frame, selectorListValue, pattern);
        }

        // Return the result
        return isValid;
    }

    /**
     * Execute the filtering logic on the selector call with the given pattern and return the result list value
     *
     * @param frame The frame to execute in
     * @param node The node to execute the selector on
     * @param pattern The pattern to perform the filtering logic
     * @return The list of the validating nodes
     */
    public ListValue executeFiltering(VirtualFrame frame, Libadalang.AdaNode node, BasePattern pattern) {
        // Get the selector list
        SelectorListValue selectorListValue = this.getSelectorList(frame, node);

        // Prepare the result
        ListValue res;

        // If the quantifier is all, verify it before returning anything
        if(this.quantifier == Quantifier.ALL) {
            if(this.isAll(frame, selectorListValue, pattern)) {
                res = this.getFilteredList(frame, selectorListValue, pattern);
            } else {
                res = new ListValue(new Libadalang.AdaNode[0]);
            }
        }

        // Else, just get the filtered list
        else {
            res = this.getFilteredList(frame, selectorListValue, pattern);
        }

        // Do the bindings
        if(this.bindingSlot > -1) {
            this.doBinding(frame, res);
        }

        // Return the result
        return res;
    }

    // ----- Class methods -----

    /**
     * Get the selector list for the selector call
     *
     * @param frame The frame to execute in
     * @param node The root node of the selector list
     * @return The selector list for the selector call
     */
    private SelectorListValue getSelectorList(VirtualFrame frame, Libadalang.AdaNode node) {
        // Get the selector and verify its type
        Object selectorObject = this.selectorExpr.executeGeneric(frame);
        if(!LKQLTypeSystemGen.isSelectorValue(selectorObject)) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_SELECTOR,
                    LKQLTypesHelper.fromJava(selectorObject),
                    this.selectorExpr
            );
        }

        // Get the arguments
        int depth = -1;
        int maxDepth = -1;
        int minDepth = -1;

        if(this.args != null) {
            for(Arg rawArg : args.getArgs()) {
                NamedArg arg = (NamedArg) rawArg;

                // If the depth is given
                switch (arg.getArgName().getName()) {
                    case "depth":
                        try {
                            depth = (int) arg.getArgExpr().executeLong(frame);
                        } catch (UnexpectedResultException e) {
                            throw LKQLRuntimeException.wrongType(
                                    LKQLTypesHelper.LKQL_INTEGER,
                                    LKQLTypesHelper.fromJava(e.getResult()),
                                    arg.getArgExpr()
                            );
                        }
                        break;

                    // If the maximum depth is given
                    case "max_depth":
                        try {
                            maxDepth = (int) arg.getArgExpr().executeLong(frame);
                        } catch (UnexpectedResultException e) {
                            throw LKQLRuntimeException.wrongType(
                                    LKQLTypesHelper.LKQL_INTEGER,
                                    LKQLTypesHelper.fromJava(e.getResult()),
                                    arg.getArgExpr()
                            );
                        }
                        break;

                    // If the minimum depth is given
                    case "min_depth":
                        try {
                            minDepth = (int) arg.getArgExpr().executeLong(frame);
                        } catch (UnexpectedResultException e) {
                            throw LKQLRuntimeException.wrongType(
                                    LKQLTypesHelper.LKQL_INTEGER,
                                    LKQLTypesHelper.fromJava(e.getResult()),
                                    arg.getArgExpr()
                            );
                        }
                        break;
                }
            }
        }

        // Cast the selector value and return the selector list
        SelectorValue selectorValue = LKQLTypeSystemGen.asSelectorValue(selectorObject);
        return selectorValue.execute(node, maxDepth, minDepth, depth);
    }

    /**
     * Verify if all node verify the pattern
     *
     * @param frame The frame to execute in
     * @param selectorListValue The list representing traversal of the selector
     * @param pattern The pattern to verify
     * @return True of all nodes of the traversal verify the pattern, false else
     */
    private boolean isAll(VirtualFrame frame, SelectorListValue selectorListValue, BasePattern pattern) {
        // Iterate on nodes
        Iterator iterator = selectorListValue.iterator();
        while (iterator.hasNext()) {
            DepthNode depthNode = (DepthNode) iterator.next();
            if(!pattern.executeNode(frame, depthNode.getNode())) return false;
        }

        // Return validation of all nodes
        return true;
    }

    /**
     * Verify if any of the node verify the pattern
     *
     * @param frame The frame to execute in
     * @param selectorListValue The list representing traversal of the selector
     * @param pattern The pattern to verify
     * @return True if there is any node that verify the pattern, false else
     */
    private boolean isAny(VirtualFrame frame, SelectorListValue selectorListValue, BasePattern pattern) {
        // Iterate on nodes
        Iterator iterator = selectorListValue.iterator();
        while (iterator.hasNext()) {
            DepthNode depthNode = (DepthNode) iterator.next();
            if(pattern.executeNode(frame, depthNode.getNode())) return true;
        }

        // Return false if no node verify the pattern
        return false;
    }

    /**
     * Get the list value filtered with the given patter
     *
     * @param frame The frame to execute in
     * @param selectorListValue The selector list value to filter
     * @param pattern The pattern for the filtering
     * @return The list value
     */
    private ListValue getFilteredList(VirtualFrame frame, SelectorListValue selectorListValue, BasePattern pattern) {
        // Prepare the result
        List<Libadalang.AdaNode> resList = new ArrayList<>();

        // Iterate on nodes
        Iterator iterator = selectorListValue.iterator();
        while(iterator.hasNext()) {
            DepthNode depthNode = (DepthNode) iterator.next();
            if(pattern.executeNode(frame, depthNode.getNode())) {
                resList.add(depthNode.getNode());
            }
        }

        // Return the result
        return new ListValue(resList.toArray(new Libadalang.AdaNode[0]));
    }

    /**
     * Do the binding process
     *
     * @param frame The frame to execute in
     * @param selectorListValue The selector list to bind
     * @param pattern The pattern to filter the list
     */
    private void doBinding(VirtualFrame frame, SelectorListValue selectorListValue, BasePattern pattern) {
        ListValue listValue = this.getFilteredList(frame, selectorListValue, pattern);
        if(this.bindingMode == Mode.LOCAL) {
            frame.setObject(this.bindingSlot, listValue);
        } else {
            LKQLLanguage.getContext(this).setGlobal(this.bindingSlot, null, listValue);
        }
    }

    /**
     * Do the binding with the already computed list
     *
     * @param frame The frame to execute in
     * @param listValue The list bind
     */
    private void doBinding(VirtualFrame frame, ListValue listValue) {
        if(this.bindingMode == Mode.LOCAL) {
            frame.setObject(this.bindingSlot, listValue);
        } else {
            LKQLLanguage.getContext(this).setGlobal(this.bindingSlot, null, listValue);
        }
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"quantifier", "mode", "slot"},
                new Object[]{this.quantifier, this.bindingMode, this.bindingSlot}
        );
    }

}
