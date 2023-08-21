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

import com.adacore.lkql_jit.exceptions.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;


/**
 * This node represents a list of list comprehension association in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ComprehensionAssocList extends LKQLNode {

    // ----- Children -----

    /**
     * The comprehension associations.
     */
    @Children
    private final ComprehensionAssoc[] compAssocs;

    // ----- Constructors -----

    /**
     * Create a new comprehension association list.
     *
     * @param location   The location of the node in the source.
     * @param compAssocs The comprehension associations.
     */
    public ComprehensionAssocList(
        SourceLocation location,
        ComprehensionAssoc[] compAssocs
    ) {
        super(location);
        this.compAssocs = compAssocs;
    }

    // ----- Getters -----

    public ComprehensionAssoc[] getCompAssocs() {
        return compAssocs;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
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
