//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLSelector;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.runtime.values.lists.LKQLSelectorList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;
import java.util.List;

/**
 * This node represents the call of a selector in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorCall extends LKQLNode {

    // ----- Attributes -----

    /** The quantifier for the selector call. */
    private final Quantifier quantifier;

    /** The slot to put the binding value in, might be -1 if there is no binding. */
    private final int bindingSlot;

    // ----- Children -----

    /** The selector to call. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr selectorExpr;

    /** The arguments for the selector call. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ArgList args;

    // ----- Constructors -----

    /**
     * Create a new selector call node.
     *
     * @param location The location of the node in the source.
     * @param quantifier The quantifier for the selector.
     * @param bindingSlot The slot of the binding.
     * @param selectorExpr The selector expression.
     * @param args The arguments for the call.
     */
    public SelectorCall(
            SourceSection location,
            Quantifier quantifier,
            int bindingSlot,
            Expr selectorExpr,
            ArgList args) {
        super(location);
        this.quantifier = quantifier;
        this.bindingSlot = bindingSlot;
        this.selectorExpr = selectorExpr;
        this.args = args;
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
     * Execute the selector on the given node and return if the tree traversal valid the given
     * pattern. TODO: Move this method and logic in the NodePatternSelector node.
     *
     * @param frame The frame to execute in.
     * @param node The node to execute the selector on.
     * @param pattern The pattern to verify.
     * @return True if the traversal verify the pattern, false else.
     */
    public boolean executeVerification(
            VirtualFrame frame, Libadalang.AdaNode node, BasePattern pattern) {
        // Get the selector list
        LKQLSelectorList selectorListValue = this.getSelectorList(frame, node);

        // Get the result of the validation
        boolean isValid;
        if (this.quantifier == Quantifier.ALL) {
            isValid = this.isAll(frame, selectorListValue, pattern);
        } else {
            isValid = this.isAny(frame, selectorListValue, pattern);
        }

        // Do the bindings
        if (this.bindingSlot > -1) {
            this.doBinding(frame, selectorListValue, pattern);
        }

        // Return the result
        return isValid;
    }

    // ----- Class methods -----

    /**
     * Get the selector list for the selector call.
     *
     * @param frame The frame to execute in.
     * @param node The root node of the selector list.
     * @return The selector list for the selector call.
     */
    private LKQLSelectorList getSelectorList(VirtualFrame frame, Libadalang.AdaNode node) {
        // Get the selector and verify its type
        Object selectorObject = this.selectorExpr.executeGeneric(frame);
        if (!LKQLTypeSystemGen.isLKQLSelector(selectorObject)) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_SELECTOR,
                    LKQLTypesHelper.fromJava(selectorObject),
                    this.selectorExpr);
        }

        // Get the arguments
        int depth = -1;
        int maxDepth = -1;
        int minDepth = -1;

        if (this.args != null) {
            for (Arg rawArg : args.getArgs()) {
                NamedArg arg = (NamedArg) rawArg;

                // If the depth is given
                switch (arg.getArgName().getName()) {
                    case Constants.DEPTH_SYMBOL:
                        try {
                            depth = (int) arg.getArgExpr().executeLong(frame);
                        } catch (UnexpectedResultException e) {
                            throw LKQLRuntimeException.wrongType(
                                    LKQLTypesHelper.LKQL_INTEGER,
                                    LKQLTypesHelper.fromJava(e.getResult()),
                                    arg.getArgExpr());
                        }
                        break;

                        // If the maximum depth is given
                    case Constants.MAX_DEPTH_SYMBOL:
                        try {
                            maxDepth = (int) arg.getArgExpr().executeLong(frame);
                        } catch (UnexpectedResultException e) {
                            throw LKQLRuntimeException.wrongType(
                                    LKQLTypesHelper.LKQL_INTEGER,
                                    LKQLTypesHelper.fromJava(e.getResult()),
                                    arg.getArgExpr());
                        }
                        break;

                        // If the minimum depth is given
                    case Constants.MIN_DEPTH_SYMBOL:
                        try {
                            minDepth = (int) arg.getArgExpr().executeLong(frame);
                        } catch (UnexpectedResultException e) {
                            throw LKQLRuntimeException.wrongType(
                                    LKQLTypesHelper.LKQL_INTEGER,
                                    LKQLTypesHelper.fromJava(e.getResult()),
                                    arg.getArgExpr());
                        }
                        break;
                }
            }
        }

        // Cast the selector value and return the selector list
        LKQLSelector selectorValue = LKQLTypeSystemGen.asLKQLSelector(selectorObject);
        return selectorValue.getList(node, maxDepth, minDepth, depth);
    }

    /**
     * Verify if all node verify the pattern.
     *
     * @param frame The frame to execute in.
     * @param selectorListValue The list representing traversal of the selector.
     * @param pattern The pattern to verify.
     * @return True of all nodes of the traversal verify the pattern, false else.
     */
    private boolean isAll(
            VirtualFrame frame, LKQLSelectorList selectorListValue, BasePattern pattern) {
        // Iterate on nodes
        Iterator iterator = selectorListValue.iterator();
        while (iterator.hasNext()) {
            Object value = iterator.next();
            if (!pattern.executeValue(frame, value)) return false;
        }

        // Return validation of all nodes
        return true;
    }

    /**
     * Verify if any of the node verify the pattern.
     *
     * @param frame The frame to execute in.
     * @param selectorListValue The list representing traversal of the selector.
     * @param pattern The pattern to verify.
     * @return True if there is any node that verify the pattern, false else.
     */
    private boolean isAny(
            VirtualFrame frame, LKQLSelectorList selectorListValue, BasePattern pattern) {
        // Iterate on nodes
        Iterator iterator = selectorListValue.iterator();
        while (iterator.hasNext()) {
            Object val = iterator.next();
            if (pattern.executeValue(frame, val)) return true;
        }

        // Return false if no node verify the pattern
        return false;
    }

    /**
     * Get the list value filtered with the given pattern.
     *
     * @param frame The frame to execute in.
     * @param selectorListValue The selector list value to filter.
     * @param pattern The pattern for the filtering.
     * @return The list value
     */
    private LKQLList getFilteredList(
            VirtualFrame frame, LKQLSelectorList selectorListValue, BasePattern pattern) {
        // Prepare the result
        List<Object> resList = new ArrayList<>();

        // Iterate on nodes
        Iterator iterator = selectorListValue.iterator();
        while (iterator.hasNext()) {
            Object value = iterator.next();
            if (pattern.executeValue(frame, value)) {
                resList.add(value);
            }
        }

        // Return the result
        return new LKQLList(resList.toArray(new Object[0]));
    }

    /**
     * Do the binding process.
     *
     * @param frame The frame to execute in.
     * @param selectorListValue The selector list to bind.
     * @param pattern The pattern to filter the list.
     */
    private void doBinding(
            VirtualFrame frame, LKQLSelectorList selectorListValue, BasePattern pattern) {
        LKQLList listValue = this.getFilteredList(frame, selectorListValue, pattern);
        this.doBinding(frame, listValue);
    }

    /**
     * Do the binding with the already computed list.
     *
     * @param frame The frame to execute in.
     * @param listValue The list bind.
     */
    private void doBinding(VirtualFrame frame, LKQLList listValue) {
        FrameUtils.writeLocal(frame, this.bindingSlot, listValue);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] {"quantifier", "slot"},
                new Object[] {this.quantifier, this.bindingSlot});
    }

    // ----- Inner classes -----

    /** This enum represents the quantifier for a selector call. */
    public enum Quantifier {
        /** The selector will match if any visited node validate. the associated pattern. */
        ANY,

        /** The selector will match if every visited nodes validate the associated pattern. */
        ALL
    }
}
