//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.list_comprehension;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a list of list comprehension association in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ComprehensionAssocList extends LKQLNode {

    // ----- Children -----

    /** The comprehension associations. */
    @Children
    private final ComprehensionAssoc[] compAssocs;

    // ----- Constructors -----

    /**
     * Create a new comprehension association list.
     *
     * @param location The location of the node in the source.
     * @param compAssocs The comprehension associations.
     */
    public ComprehensionAssocList(SourceSection location, ComprehensionAssoc[] compAssocs) {
        super(location);
        this.compAssocs = compAssocs;
    }

    // ----- Getters -----

    public ComprehensionAssoc[] getCompAssocs() {
        return compAssocs;
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
     * Get the collections to iterate on in the list comprehension.
     *
     * @param frame The frame to execute the in.
     * @return The collection array.
     */
    public Iterable[] executeCollections(VirtualFrame frame) {
        // Prepare the result
        Iterable[] res = new Iterable[this.compAssocs.length];

        for (int i = 0; i < res.length; i++) {
            res[i] = this.compAssocs[i].executeCollection(frame);
        }

        // Return the iterable list
        return res;
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
