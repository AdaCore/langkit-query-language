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

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.nodes.patterns.chained_patterns.ChainedNodePattern;
import com.adacore.lkql_jit.runtime.values.DepthNode;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.runtime.values.NodeNull;
import com.adacore.lkql_jit.runtime.values.SelectorValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
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
            SourceLocation location,
            Kind kind,
            boolean followGenerics,
            Expr throughExpr,
            Expr fromExpr,
            BasePattern pattern) {
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
        SelectorValue through = null;
        Libadalang.AdaNode[] fromNodes;

        // Get the through expression
        if (this.throughExpr != null) {
            try {
                through = this.throughExpr.executeSelector(frame);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_SELECTOR,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.throughExpr);
            }
        }

        // If there is a "from" expression
        if (this.fromExpr != null) {
            Object fromObject = this.fromExpr.executeGeneric(frame);

            // If the "from" is a sole node
            if (LKQLTypeSystemGen.isAdaNode(fromObject)) {
                fromNodes = new Libadalang.AdaNode[1];
                fromNodes[0] = LKQLTypeSystemGen.asAdaNode(fromObject);
            }

            // Else, if the "from" is a list of node
            else if (LKQLTypeSystemGen.isListValue(fromObject)) {
                // Verify the content of the list
                ListValue fromList = LKQLTypeSystemGen.asListValue(fromObject);
                fromNodes = new Libadalang.AdaNode[(int) fromList.size()];
                for (int i = 0; i < fromList.size(); i++) {
                    try {
                        fromNodes[i] = LKQLTypeSystemGen.expectAdaNode(fromList.get(i));
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
            List<Libadalang.AdaNode> resNodes = new LinkedList<>();

            // For each root node, explore it and return the result
            for (int i = fromNodes.length - 1; i >= 0; i--) {
                Iterable nodes = this.createNodeIterable(fromNodes[i], through);
                List<Libadalang.AdaNode> result = this.exploreAll(frame, nodes.iterator());
                for (int j = 0; j < result.size(); j++) resNodes.add(result.get(j));
            }

            // Return the result list value
            return new ListValue(resNodes.toArray(new Libadalang.AdaNode[0]));
        }

        // If the query mode is first
        else {
            for (int i = fromNodes.length - 1; i >= 0; i--) {
                Iterable nodes = this.createNodeIterable(fromNodes[i], through);
                Libadalang.AdaNode res = this.exploreFirst(frame, nodes.iterator());
                if (!res.isNone()) return res;
            }

            // Return the null value if there is none
            return NodeNull.getInstance();
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
    private List<Libadalang.AdaNode> exploreAll(VirtualFrame frame, Iterator nodeIterator) {
        // Create the result list
        List<Libadalang.AdaNode> resList = new ArrayList<>();

        // Iterate on all node in the iterator
        while (nodeIterator.hasNext()) {
            // Get the current node
            Libadalang.AdaNode adaNode =
                    this.throughExpr == null
                            ? (Libadalang.AdaNode) nodeIterator.next()
                            : ((DepthNode) nodeIterator.next()).getNode();

            // If the pattern is a chained one
            if (this.pattern instanceof ChainedNodePattern chainedNodePattern) {
                Libadalang.AdaNode[] res = chainedNodePattern.executeChained(frame, adaNode);
                if (res != null) {
                    for (Libadalang.AdaNode resNode : res) resList.add(resNode);
                }
            }

            // Else, the pattern is a basic one
            else {
                if (this.pattern.executeNode(frame, adaNode)) {
                    resList.add(adaNode);
                }
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
    private Libadalang.AdaNode exploreFirst(VirtualFrame frame, Iterator nodeIterator) {
        // Iterate on all node in the iterator
        while (nodeIterator.hasNext()) {
            // Get the current node
            Libadalang.AdaNode adaNode =
                    this.throughExpr == null
                            ? (Libadalang.AdaNode) nodeIterator.next()
                            : ((DepthNode) nodeIterator.next()).getNode();

            // If the pattern is a chained one
            if (this.pattern instanceof ChainedNodePattern chainedNodePattern) {
                Libadalang.AdaNode[] res = chainedNodePattern.executeChained(frame, adaNode);
                if (res != null && res.length > 0) {
                    return res[0];
                }
            }

            // Else, the pattern is a basic one
            else {
                if (this.pattern.executeNode(frame, adaNode)) {
                    return adaNode;
                }
            }
        }

        // Return the null value
        return NodeNull.getInstance();
    }

    /**
     * Create a node iterator with the given root and the given through method
     *
     * @param root The root of the iterator
     * @param through The method to go through the iteration, if null this is a default children
     *     exploration
     * @return The iterator for the node exploration
     */
    private Iterable createNodeIterable(Libadalang.AdaNode root, SelectorValue through) {
        if (through == null) {
            return new ChildIterable(root, this.followGenerics);
        } else {
            return through.execute(root);
        }
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"queryKind"}, new Object[] {this.kind});
    }

    // ----- Inner classes -----

    /** This enum represents a query kind. */
    public enum Kind {
        /** Select all nodes matching the query pattern. */
        ALL,

        /** Select only the first node matching the query pattern. */
        FIRST
    }

    /** This class is a tool to represent the tree exploration for a default query */
    private static final class ChildIterable implements Iterable {

        // ----- Attributes -----

        /** The root of the iterable */
        private final Libadalang.AdaNode root;

        /** Whether the traversal should follow the generic instantiations */
        private final boolean followGenerics;

        // ----- Constructors -----

        /**
         * Create a new child iterable
         *
         * @param root The root node
         * @param followGenerics Whether the traversal should follow the ada generic instantiations
         */
        public ChildIterable(Libadalang.AdaNode root, boolean followGenerics) {
            this.root = root;
            this.followGenerics = followGenerics;
        }

        // ----- Override methods -----

        /**
         * @see com.adacore.lkql_jit.runtime.values.interfaces.Iterable#iterator()
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

        @Override
        public boolean contains(Object elem) {
            return false;
        }

        @Override
        public boolean internalEquals(LKQLValue o) {
            return false;
        }
    }

    /** This class is the iterator for a query without through */
    public static final class ChildIterator implements Iterator {

        // ----- Attributes -----

        /** The queue to explore the children */
        private final LinkedList<Libadalang.AdaNode> queue;

        /** Whether the iterator should follow the generic instantiations */
        private final boolean followGenerics;

        // ----- Constructors -----

        /**
         * Create a new child iterator for given root
         *
         * @param root The root of the exploration
         * @param followGenerics If the iterator should follow the ada generic instantiation
         */
        public ChildIterator(Libadalang.AdaNode root, boolean followGenerics) {
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
            Libadalang.AdaNode next = this.queue.remove(0);

            // Add the node child in the queue
            int childrenCount = next.getChildrenCount();
            for (int i = childrenCount - 1; i >= 0; i--) {
                Libadalang.AdaNode child = next.getChild(i);
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
                } else if (next instanceof Libadalang.BodyStub stub
                        && inGenericInstantiation(next)) {
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
        private static boolean inGenericInstantiation(Libadalang.AdaNode node) {
            return node.pGenericInstantiations().size() > 0;
        }

        // ----- Un-needed methods -----

        @Override
        public void reset() {}
    }
}
