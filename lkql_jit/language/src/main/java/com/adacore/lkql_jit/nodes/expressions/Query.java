//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.LKQLFunction;
import com.adacore.lkql_jit.values.LKQLNull;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.adacore.lkql_jit.values.lists.LKQLArrayList;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;

/**
 * This node represents a query in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class Query extends Expr {

    // ----- Attributes -----

    /** The kind of the query */
    private final Kind kind;

    // ----- Children -----

    /** The "through" expression of the query */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr throughExpr;

    /** The "from" expression of the query */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr fromExpr;

    /** The pattern to filter the query result */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Pattern pattern;

    // ----- Constructors -----

    /**
     * Create a new query node
     *
     * @param location The location of the node in the source
     * @param kind The kind of the query
     * @param followGenerics Whether the tree traversal should follow the generic instantiations
     * @param throughExpr The expression of the "through" element
     * @param fromExpr The "from" expression (might be null)
     * @param pattern The pattern of the query node
     */
    public Query(
        SourceSection location,
        Kind kind,
        Expr throughExpr,
        Expr fromExpr,
        Pattern pattern
    ) {
        super(location);
        this.kind = kind;
        this.throughExpr = throughExpr;
        this.fromExpr = fromExpr;
        this.pattern = pattern;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        final var through = executeThrough(frame);
        final var fromNodes = executeFromNodes(frame);

        // If the query mode is all
        return switch (this.kind) {
            case Kind.ALL -> {
                final var resNodes = new ArrayList<LangkitSupport.NodeInterface>();

                // Core search loop
                for (int i = fromNodes.length - 1; i >= 0; i--) {
                    final var nodeIterator = createNodeIterator(fromNodes[i], through);
                    while (nodeIterator.hasNext()) {
                        final var node = (LangkitSupport.NodeInterface) nodeIterator.next();
                        if (this.pattern.executeValue(frame, node)) {
                            resNodes.add(node); // add to accumulator
                        }
                    }
                }
                // Return accumulated results
                yield new LKQLArrayList(resNodes.toArray(new LangkitSupport.NodeInterface[0]));
            }
            case Kind.FIRST -> {
                // Core search loop
                for (int i = fromNodes.length - 1; i >= 0; i--) {
                    final var nodeIterator = createNodeIterator(fromNodes[i], through);
                    while (nodeIterator.hasNext()) {
                        final var node = (LangkitSupport.NodeInterface) nodeIterator.next();
                        if (this.pattern.executeValue(frame, node)) {
                            yield node; // early return
                        }
                    }
                }
                // Return the null value if there is none
                yield LKQLNull.INSTANCE;
            }
        };
    }

    private LKQLFunction executeThrough(VirtualFrame frame) {
        try {
            return this.throughExpr.executeFunction(frame);
        } catch (UnexpectedResultException e) {
            throw LKQLRuntimeError.wrongType(
                LKQLTypesHelper.LKQL_FUNCTION,
                LKQLTypesHelper.fromJava(e.getResult()),
                this.throughExpr
            );
        }
    }

    private LangkitSupport.NodeInterface[] executeFromNodes(VirtualFrame frame) {
        // If there is no "from" expression, we get the default roots
        if (fromExpr == null) return LKQLLanguage.getContext(pattern).allUnitsRoots();

        // If there is a "from" expression
        Object fromObject = fromExpr.executeGeneric(frame);

        // from is a single node
        if (LKQLTypeSystemGen.isNodeInterface(fromObject)) {
            final var fromNode = LKQLTypeSystemGen.asNodeInterface(fromObject);
            return new LangkitSupport.NodeInterface[] { fromNode };
        }

        // from is a list
        if (LKQLTypeSystemGen.isLKQLList(fromObject)) {
            final var fromList = LKQLTypeSystemGen.asLKQLList(fromObject);
            try {
                final var fromNodes = new LangkitSupport.NodeInterface[(int) fromList.size()];
                // Verify the content of the list
                for (int i = 0; i < fromList.size(); i++) {
                    fromNodes[i] = LKQLTypeSystemGen.expectNodeInterface(fromList.get(i));
                }
                return fromNodes;
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeError.wrongFromList(fromExpr);
            }
        }

        // from type is invalid
        throw LKQLRuntimeError.wrongFrom(fromExpr);
    }

    // ----- Class methods -----

    /**
     * Create a node iterator with the given root and the given through method
     *
     * @param root The root of the iterator
     * @param through The method to go through the iteration, if null this is a default children
     *     exploration
     * @return The iterator for the node exploration
     */
    private Iterator createNodeIterator(LangkitSupport.NodeInterface root, LKQLFunction through) {
        var selectorList = through.getCallTarget().call(null, through.closure, root, -1l, -1l, -1l);
        if (selectorList instanceof LKQLStream stream) {
            return stream.iterator();
        } else {
            throw LKQLRuntimeError.wrongType(
                LKQLTypesHelper.LKQL_STREAM,
                LKQLTypesHelper.fromJava(selectorList),
                throughExpr
            );
        }
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[] { "queryKind" },
            new Object[] { this.kind }
        );
    }

    // ----- Inner classes -----

    /** This enum represents a query kind. */
    public enum Kind {
        /** Select all nodes matching the query pattern. */
        ALL,

        /** Select only the first node matching the query pattern. */
        FIRST,
    }
}
