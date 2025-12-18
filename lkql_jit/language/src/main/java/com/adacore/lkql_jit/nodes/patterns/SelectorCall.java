//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.LKQLSelector;
import com.adacore.lkql_jit.values.lists.LKQLSelectorList;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the call of a selector in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorCall extends LKQLNode {

    // ----- Attributes -----

    /** The quantifier for the selector call. */
    private final Quantifier quantifier;

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
     * @param selectorExpr The selector expression.
     * @param args The arguments for the call.
     */
    public SelectorCall(
        SourceSection location,
        Quantifier quantifier,
        Expr selectorExpr,
        ArgList args
    ) {
        super(location);
        this.quantifier = quantifier;
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
    public boolean executeVerification(VirtualFrame frame, Object node, Pattern pattern) {
        // Get the selector list
        LKQLSelectorList selectorListValue = this.getSelectorList(frame, node);

        // Get the result of the validation
        boolean isValid;
        if (this.quantifier == Quantifier.ALL) {
            isValid = this.isAll(frame, selectorListValue, pattern);
        } else {
            isValid = this.isAny(frame, selectorListValue, pattern);
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
    private LKQLSelectorList getSelectorList(VirtualFrame frame, Object node) {
        // Get the selector and verify its type
        Object selectorObject = this.selectorExpr.executeGeneric(frame);
        if (!LKQLTypeSystemGen.isLKQLSelector(selectorObject)) {
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

        if (this.args != null) {
            for (Arg rawArg : args.getArgs()) {
                NamedArg arg = (NamedArg) rawArg;

                // If the depth is given
                switch (arg.getArgStringName()) {
                    case Constants.DEPTH_SYMBOL:
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
                    case Constants.MAX_DEPTH_SYMBOL:
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
                    case Constants.MIN_DEPTH_SYMBOL:
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
    private boolean isAll(VirtualFrame frame, LKQLSelectorList selectorListValue, Pattern pattern) {
        // Iterate on nodes
        for (int i = 0; i < selectorListValue.size(); i++) {
            if (!pattern.executeValue(frame, selectorListValue.get(i))) return false;
        }

        // Return true if all nodes verify the pattern
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
    private boolean isAny(VirtualFrame frame, LKQLSelectorList selectorListValue, Pattern pattern) {
        // Iterate on nodes.
        // Here we use an iterator to compute the iterator list content in a
        // lazy way.
        var iterator = selectorListValue.iterator();
        while (iterator.hasNext()) {
            var val = iterator.next();
            if (pattern.executeValue(frame, val)) return true;
        }

        // Return false if no node verify the pattern
        return false;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "quantifier" },
                new Object[] { this.quantifier }
            );
    }

    // ----- Inner classes -----

    /** This enum represents the quantifier for a selector call. */
    public enum Quantifier {
        /** The selector will match if any visited node validate. the associated pattern. */
        ANY,

        /** The selector will match if every visited nodes validate the associated pattern. */
        ALL,
    }
}
