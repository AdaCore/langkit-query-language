//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
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
import java.util.LinkedList;

/**
 * This node represents a query in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class Query extends Expr {

    // ----- Attributes -----

    /** The kind of the query */
    private final Kind kind;

    /** Whether the traversal should follow the generic instantiations */
    private final boolean followGenerics;

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
        boolean followGenerics,
        Expr throughExpr,
        Expr fromExpr,
        Pattern pattern
    ) {
        super(location);
        this.kind = kind;
        this.followGenerics = followGenerics;
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
        final var through = this.throughExpr != null ? executeThrough(frame) : null;
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
        if (through == null) return new ChildIterator(root, followGenerics);

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

    /** This class is the iterator for a query without through */
    public static final class ChildIterator implements Iterator {

        // ----- Attributes -----

        /** The queue to explore the children */
        private final LinkedList<LangkitSupport.NodeInterface> queue;

        /** Whether the iterator should follow the generic instantiations */
        private final boolean followGenerics;

        // ----- Constructors -----

        /**
         * Create a new child iterator for given root
         *
         * @param root The root of the exploration
         * @param followGenerics If the iterator should follow the ada generic instantiation
         */
        public ChildIterator(LangkitSupport.NodeInterface root, boolean followGenerics) {
            this.queue = new LinkedList<>();
            this.queue.add(root);
            this.followGenerics = followGenerics;
        }

        // ----- Override methods -----

        /**
         * @see Iterator#hasNext()
         */
        @Override
        public boolean hasNext() {
            return this.queue.size() > 0;
        }

        /**
         * @see Iterator#next()
         */
        @Override
        public Object next() {
            // Get the next node
            LangkitSupport.NodeInterface next = this.queue.remove(0);

            // Add the node child in the queue
            int childrenCount = next.getChildrenCount();
            for (int i = childrenCount - 1; i >= 0; i--) {
                LangkitSupport.NodeInterface child = next.getChild(i);
                if (!child.isNone()) {
                    this.queue.add(0, child);
                }
            }

            // Test if the iterator should follow the generic instantiations
            if (this.followGenerics) {
                if (next instanceof Libadalang.GenericInstantiation genInst) {
                    // If the node is a generic instantiation, traverse the instantiated generic
                    Libadalang.BasicDecl genDecl = genInst.pDesignatedGenericDecl();
                    Libadalang.BodyNode genBody = genDecl.pBodyPartForDecl(false);
                    this.queue.add(genDecl);
                    if (!genBody.isNone()) {
                        this.queue.add(genBody);
                    }
                } else if (
                    next instanceof Libadalang.BodyStub stub && inGenericInstantiation(next)
                ) {
                    // If this node is a body stub and we are currently traversing a generic
                    // instantiation,
                    // we should also traverse the stub's completion.
                    // TODO: can we keep track of whether we are in an instantiation like we do in
                    // NodeCheckerFunction
                    // instead of relying on the `pGenericInstantiations()` function ?
                    this.queue.add(stub.pNextPartForDecl(false));
                }
            }

            // return the result
            return next;
        }

        /**
         * Return whether the given node is inside an instantiated generic.
         *
         * @param node The node to check
         */
        private static boolean inGenericInstantiation(LangkitSupport.NodeInterface node) {
            // TODO: Genericize LKQL issue #500. Cannot interface Ada specific calls.
            return ((Libadalang.AdaNode) node).pGenericInstantiations().length > 0;
        }

        // ----- Un-needed methods -----

        @Override
        public void reset() {}
    }
}
