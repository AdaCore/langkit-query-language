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

package com.adacore.lkql_jit.nodes.expressions.list_comprehension;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.ListComprehensionRootNode;
import com.adacore.lkql_jit.runtime.values.LazyListValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.adacore.lkql_jit.utils.util_classes.Iterator;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a list comprehension in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class ListComprehension extends Expr {

    // ----- Attributes -----

    /** The root node of the list comprehension */
    private final ListComprehensionRootNode rootNode;

    // ----- Children -----

    /** The generators of the list comprehension */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private ListCompAssocList generators;

    // -----  Constructors -----

    /**
     * Create a new list comprehension node
     *
     * @param location The location of the node in the source
     * @param descriptor The frame descriptor for the root node
     * @param expr The result expression of the list comprehension
     * @param generators The generators of the list comprehension
     * @param guard The guard of the list comprehension
     */
    public ListComprehension(
            SourceLocation location,
            FrameDescriptor descriptor,
            Expr expr,
            ListCompAssocList generators,
            Expr guard
    ) {
        super(location);
        this.generators = generators;
        int[] slots = new int[generators.getCompAssocs().length];
        for(int i = 0 ; i < slots.length ; i++) {
            slots[i] = generators.getCompAssocs()[i].getSlot();
        }
        this.rootNode = new ListComprehensionRootNode(
                LKQLLanguage.getLanguage(this),
                descriptor,
                slots,
                guard,
                expr
        );
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeLazyList(frame);
    }

    /** @see com.adacore.lkql_jit.nodes.expressions.Expr#executeLazyList(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public LazyListValue executeLazyList(VirtualFrame frame) {
        // Get the iterables for the list comprehension
        Iterable[] iterables = this.generators.executeCollections(frame);

        // Create the closure for the result
        Closure closure = new Closure(frame.materialize());

        // Get the result size and prepare the result
        int resultSize = 1;
        for(Iterable iterable : iterables) {
            resultSize *= iterable.size();
        }

        // Verify that the result size is strictly positive
        if(resultSize < 1) {
            return new LazyListValue(
                    closure,
                    this.rootNode,
                    new Object[0][]
            );
        }

        // Prepare the array of arguments for each iteration in the list comprehension
        Object[][] argsList = new Object[resultSize][];

        // Initialize the working variables
        Iterator[] iterators = new Iterator[iterables.length];
        Object[] valueBuffer = new Object[iterables.length];
        for(int i = 0 ; i < iterables.length ; i++) {
            iterators[i] = iterables[i].iterator();
            valueBuffer[i] = iterators[i].next();
        }

        // While collections are not fully
        int i = 0;
        do {
            argsList[i++] = valueBuffer.clone();
        } while(this.increaseIndexes(iterators, valueBuffer));

        // Return the result of the list comprehension as a lazy list
        return new LazyListValue(
                closure,
                this.rootNode,
                argsList
        );
    }

    // ----- Class methods -----

    /**
     * Increase the indexes for the
     *
     * @param iterators The iterators containing the current iteration information
     * @param valueBuffer The buffer to put the values in
     * @return True if the indexes have been increased
     */
    private boolean increaseIndexes(Iterator[] iterators, Object[] valueBuffer) {
        for(int i = iterators.length - 1 ; i >= 0 ; i--) {
            // If the iterator has a next value
            if(iterators[i].hasNext()) {
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

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }

}
