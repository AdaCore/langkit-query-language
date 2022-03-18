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

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.declarations.Import;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;


/**
 * This node represents the list of all top level instructions of a LKQL program
 * It's the "highest" node in a LKQL AST and is the starting point of the program
 *
 * @author Hugo GUERRIER
 */
public final class TopLevelList extends LKQLNode {

    // ----- Attributes -----

    /** The size of the global scope */
    private final int globalScopeSize;

    /** The number of slot to export */
    private final int symbolNumber;

    // ----- Children -----

    /** The rule importation nodes */
    @Children
    private Import[] ruleImports;

    /** The list of nodes representing the program */
    @Children
    private final LKQLNode[] program;

    // ----- Constructors -----

    /**
     * Create a new top level list node
     *
     * @param location The location of the node in the source
     * @param globalScopeSize The size of the global scope stack
     * @param symbolNumber The number of symbol to export in the namespace
     * @param nodes The nodes to execute in the top level
     */
    public TopLevelList(
            SourceLocation location,
            int globalScopeSize,
            int symbolNumber,
            LKQLNode[] nodes
    ) {
        super(location);
        this.program = nodes;
        this.globalScopeSize = globalScopeSize;
        this.symbolNumber = symbolNumber;
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Get the language context and initialize it
        LKQLContext context = LKQLLanguage.getContext(this);
        context.getGlobalValues().initScope(this.globalScopeSize, this.symbolNumber);

        // If there is rule imports, run them
        if(this.ruleImports != null && this.ruleImports.length > 0) {
            for(Import ruleImport : this.ruleImports) {
                ruleImport.executeGeneric(frame);
            }
        }

        // Execute the nodes of the program
        for(LKQLNode node : program) {
            node.executeGeneric(frame);
        }

        // Prepare the result of the script, the namespace of the program
        Object res = context.getEnv().asGuestValue(
                context.getGlobalValues().export()
        );

        // Close the global scope
        context.getGlobalValues().finalizeScope();

        // Return the result
        return res;
    }

    // ----- Class methods -----

    /**
     * Add rule modules to import
     *
     * @param importNames The names of the rule modules to import
     */
    public void addRuleImports(String[] importNames) {
        this.ruleImports = new Import[importNames.length];
        for(int i = 0 ; i < importNames.length ; i++) {
            this.ruleImports[i] = new Import(
                    null,
                    importNames[i],
                    -1
            );
        }
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"globalScopeSize", "symbolNumber"},
                new Object[]{this.globalScopeSize, this.symbolNumber}
        );
    }

}
