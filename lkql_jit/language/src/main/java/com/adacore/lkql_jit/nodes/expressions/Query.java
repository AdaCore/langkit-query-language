//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLSelector;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

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
    private BasePattern pattern;

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
        BasePattern pattern
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
        // Prepare the working variable
        LKQLSelector through = null;
        LangkitSupport.NodeInterface[] fromNodes;

        // Get the through expression
        if (this.throughExpr != null) {
            try {
                through = this.throughExpr.executeSelector(frame);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_SELECTOR,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.throughExpr
                );
            }
        }

        // If there is a "from" expression
        if (this.fromExpr != null) {
            Object fromObject = this.fromExpr.executeGeneric(frame);

            // If the "from" is a sole node
            if (LKQLTypeSystemGen.isNodeInterface(fromObject)) {
                fromNodes = new LangkitSupport.NodeInterface[1];
                fromNodes[0] = LKQLTypeSystemGen.asNodeInterface(fromObject);
            }
            // Else, if the "from" is a list of node
            else if (LKQLTypeSystemGen.isLKQLList(fromObject)) {
                // Verify the content of the list
                LKQLList fromList = LKQLTypeSystemGen.asLKQLList(fromObject);
                fromNodes = new LangkitSupport.NodeInterface[(int) fromList.size()];
                for (int i = 0; i < fromList.size(); i++) {
                    try {
                        fromNodes[i] = LKQLTypeSystemGen.expectNodeInterface(fromList.get(i));
                    } catch (UnexpectedResultException e) {
                        throw LKQLRuntimeException.wrongFromList(this.fromExpr);
                    }
                }
            }
            // Else, throw an exception on the "from" expression type
            else {
                throw LKQLRuntimeException.wrongFrom(this.fromExpr);
            }
        }
        // Else, there is no "from" expression, we get the default roots
        else {
            fromNodes = LKQLLanguage.getContext(this.pattern).getAllUnitsRoots();
        }

        // If the query mode is all
        if (this.kind == Kind.ALL) {
            // Prepare the result
            List<LangkitSupport.NodeInterface> resNodes = new ArrayList<>();

            // For each root node, explore it and return the result
            for (int i = fromNodes.length - 1; i >= 0; i--) {
                Iterable nodes = this.createNodeIterable(fromNodes[i], through);
                List<LangkitSupport.NodeInterface> result =
                    this.exploreAll(frame, nodes.iterator());
                for (int j = 0; j < result.size(); j++) resNodes.add(result.get(j));
            }

            // Return the result list value
            return new LKQLList(resNodes.toArray(new LangkitSupport.NodeInterface[0]));
        }
        // If the query mode is first
        else {
            for (int i = fromNodes.length - 1; i >= 0; i--) {
                Iterable nodes = this.createNodeIterable(fromNodes[i], through);
                LangkitSupport.NodeInterface res = this.exploreFirst(frame, nodes.iterator());
                if (!res.isNone()) return res;
            }

            // Return the null value if there is none
            return LKQLNull.INSTANCE;
        }
    }

    // ----- Class methods -----

    /**
     * Explore a node iterator and get all the matching nodes
     *
     * @param frame The frame to execute in
     * @param nodeIterator The node iterator to explore
     * @return The list of the nodes that matches the pattern
     */
    private List<LangkitSupport.NodeInterface> exploreAll(
        VirtualFrame frame,
        Iterator nodeIterator
    ) {
        // Create the result list
        List<LangkitSupport.NodeInterface> resList = new ArrayList<>();

        // Iterate on all node in the iterator
        while (nodeIterator.hasNext()) {
            // Get the current node
            LangkitSupport.NodeInterface node = (LangkitSupport.NodeInterface) nodeIterator.next();

            if (this.pattern.executeValue(frame, node)) {
                resList.add(node);
            }
        }

        // Return the result list
        return resList;
    }

    /**
     * Explore a node iterator and get the first matching node
     *
     * @param frame The frame to execute in
     * @param nodeIterator The node iterator to explore
     * @return The first matching node, null if none
     */
    private LangkitSupport.NodeInterface exploreFirst(VirtualFrame frame, Iterator nodeIterator) {
        // Iterate on all node in the iterator
        while (nodeIterator.hasNext()) {
            // Get the current node
            LangkitSupport.NodeInterface node = (LangkitSupport.NodeInterface) nodeIterator.next();

            if (this.pattern.executeValue(frame, node)) {
                return node;
            }
        }

        // Return the null value
        return LKQLNull.INSTANCE;
    }

    /**
     * Create a node iterator with the given root and the given through method
     *
     * @param root The root of the iterator
     * @param through The method to go through the iteration, if null this is a default children
     *     exploration
     * @return The iterator for the node exploration
     */
    private Iterable createNodeIterable(LangkitSupport.NodeInterface root, LKQLSelector through) {
        if (through == null) {
            return new ChildIterable(root, this.followGenerics);
        } else {
            return through.getList(root);
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

    /** This class is a tool to represent the tree exploration for a default query */
    private static final class ChildIterable implements Iterable {

        // ----- Attributes -----

        /** The root of the iterable */
        private final LangkitSupport.NodeInterface root;

        /** Whether the traversal should follow the generic instantiations */
        private final boolean followGenerics;

        // ----- Constructors -----

        /**
         * Create a new child iterable
         *
         * @param root The root node
         * @param followGenerics Whether the traversal should follow the ada generic instantiations
         */
        public ChildIterable(LangkitSupport.NodeInterface root, boolean followGenerics) {
            this.root = root;
            this.followGenerics = followGenerics;
        }

        // ----- Override methods -----

        /**
         * @see Iterable#iterator()
         */
        @Override
        public Iterator iterator() {
            return new ChildIterator(this.root, this.followGenerics);
        }

        // ----- Un-needed methods -----

        @Override
        public long size() {
            return -1;
        }
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
         * @see com.adacore.lkql_jit.utils.Iterator#hasNext()
         */
        @Override
        public boolean hasNext() {
            return this.queue.size() > 0;
        }

        /**
         * @see com.adacore.lkql_jit.utils.Iterator#next()
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
