//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.list_comprehension;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.ListComprehensionRootNode;
import com.adacore.lkql_jit.nodes.utils.CreateClosureNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.lists.LKQLLazyList;
import com.adacore.lkql_jit.runtime.values.lists.LKQLListComprehension;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.adacore.lkql_jit.utils.Iterator;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a list comprehension in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ListComprehension extends Expr {

    // ----- Attributes -----

    /** Slots for the list comprehension parameters. */
    private final int[] slots;

    /** Descriptor of the frame to create. */
    private final FrameDescriptor frameDescriptor;

    /** Descriptor for the values to close. */
    private final ClosureDescriptor closureDescriptor;

    // ----- Children -----

    /** Generators of the list comprehension. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ComprehensionAssocList generators;

    /** Result expression of the list comprehension. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr expr;

    /** Guard expression of the list comprehension. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr guard;

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private CreateClosureNode createClosureNode;

    /** Root node of the list comprehension. */
    private final ListComprehensionRootNode rootNode;

    // -----  Constructors -----

    /**
     * Create a new list comprehension node.
     *
     * @param location The location of the node in the source.
     * @param frameDescriptor The frame descriptor for the root node.
     * @param closureDescriptor The descriptor for the closure.
     * @param expr The result expression of the list comprehension.
     * @param generators The generators of the list comprehension.
     * @param guard The guard of the list comprehension.
     */
    public ListComprehension(
        final SourceSection location,
        final FrameDescriptor frameDescriptor,
        final ClosureDescriptor closureDescriptor,
        final ComprehensionAssocList generators,
        final Expr expr,
        final Expr guard
    ) {
        super(location);
        this.frameDescriptor = frameDescriptor;
        this.closureDescriptor = closureDescriptor;
        this.generators = generators;
        this.slots = new int[generators.getCompAssocs().length];
        for (int i = 0; i < this.slots.length; i++) {
            this.slots[i] = generators.getCompAssocs()[i].getSlot();
        }
        this.expr = expr;
        this.guard = guard;
        this.rootNode = createRootNode();
        this.createClosureNode = new CreateClosureNode(closureDescriptor);
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeLazyList(frame);
    }

    @Override
    public LKQLLazyList executeLazyList(VirtualFrame frame) {
        // Get the iterables for the list comprehension
        Iterable[] iterables = this.generators.executeCollections(frame);

        // Get the result size and prepare the result
        int resultSize = 1;
        for (Iterable iterable : iterables) {
            resultSize *= (int) iterable.size();
        }

        // Verify that the result size is strictly positive
        assert resultSize >= 0;
        if (resultSize == 0) {
            return new LKQLListComprehension(this.rootNode, Closure.EMPTY, new Object[0][]);
        }

        // Prepare the array of arguments for each iteration in the list comprehension
        Object[][] argsList = new Object[resultSize][];

        // Initialize the working variables
        Iterator[] iterators = new Iterator[iterables.length];
        Object[] valueBuffer = new Object[iterables.length];
        for (int i = 0; i < iterables.length; i++) {
            iterators[i] = iterables[i].iterator();
            valueBuffer[i] = iterators[i].next();
        }

        // While collections are not fully
        int i = 0;
        do {
            argsList[i++] = valueBuffer.clone();
        } while (this.increaseIndexes(iterators, valueBuffer));

        // Return the result of the list comprehension as a lazy list
        return new LKQLListComprehension(this.rootNode, createClosureNode.execute(frame), argsList);
    }

    // ----- Class methods -----

    /**
     * Increase the iteration indexes.
     *
     * @param iterators The iterators containing the current iteration information.
     * @param valueBuffer The buffer to put the values in.
     * @return True if the indexes have been increased.
     */
    private boolean increaseIndexes(Iterator[] iterators, Object[] valueBuffer) {
        for (int i = iterators.length - 1; i >= 0; i--) {
            // If the iterator has a next value
            if (iterators[i].hasNext()) {
                valueBuffer[i] = iterators[i].next();
                return true;
            }
            // Else reset the iterator and go to the next one
            else {
                iterators[i].reset();
                valueBuffer[i] = iterators[i].next();
            }
        }
        return false;
    }

    /**
     * Create a root node for the list comprehension.
     *
     * @return The root node for the list comprehension execution.
     */
    private ListComprehensionRootNode createRootNode() {
        return new ListComprehensionRootNode(
            LKQLLanguage.getLanguage(this),
            this.frameDescriptor,
            this.guard,
            this.expr
        );
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
