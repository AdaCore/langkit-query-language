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
import com.adacore.lkql_jit.runtime.values.NamespaceValue;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


/**
 * This node represents the list of all top level instructions of a LKQL program
 * It's the "highest" node in a LKQL AST and is the starting point of the program
 *
 * @author Hugo GUERRIER
 */
public final class TopLevelList extends LKQLNode {

    // ----- Attributes -----

    /**
     * Descriptor of the top level frame.
     */
    private final FrameDescriptor frameDescriptor;

    // ----- Children -----

    /**
     * The rule importation nodes.
     */
    @Children
    private Import[] ruleImports;

    /**
     * The list of nodes representing the LKQL program.
     */
    @Children
    private final LKQLNode[] program;

    // ----- Constructors -----

    /**
     * Create a new top level list node.
     *
     * @param location        The location of the node in the source.
     * @param frameDescriptor The frame descriptor for the top level.
     * @param nodes           The nodes to execute in the top level.
     */
    public TopLevelList(
        SourceLocation location,
        FrameDescriptor frameDescriptor,
        LKQLNode[] nodes
    ) {
        super(location);
        this.frameDescriptor = frameDescriptor;
        this.program = nodes;
    }

    // ----- Getters -----

    public FrameDescriptor getFrameDescriptor() {
        return this.frameDescriptor;
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // If there is rule imports, run them
        if (this.ruleImports != null) {
            for (Import ruleImport : this.ruleImports) {
                ruleImport.executeGeneric(frame);
            }
        }

        // Execute the nodes of the program
        for (LKQLNode node : program) {
            node.executeGeneric(frame);
        }

        // Get the language context and initialize it
        final LKQLContext context = LKQLLanguage.getContext(this);

        // Return the namespace corresponding to the program execution
        return context.getEnv().asGuestValue(
            NamespaceValue.create(frame.materialize())
        );
    }

    // ----- Class methods -----

    /**
     * Add all required rule importing nodes.
     */
    @CompilerDirectives.TruffleBoundary
    public void addRuleImports() {
        // Get the current context
        LKQLContext context = LKQLLanguage.getContext(this);

        // Get the directories to fetch the rules from
        final String[] ruleDirectories = context.getRuleDirectories();
        final List<Import> ruleImports = new ArrayList<>();

        // Get all rule modules import nodes
        for (String dirName : ruleDirectories) {
            File ruleDirectory = new File(dirName);
            if (ruleDirectory.isDirectory() && ruleDirectory.canRead()) {
                final File[] ruleDirectoryFiles = ruleDirectory.listFiles(f -> f.canRead() && f.getName().endsWith(Constants.LKQL_EXTENSION));
                if (ruleDirectoryFiles != null) {
                    ruleImports.addAll(
                        Arrays.stream(ruleDirectoryFiles)
                            .filter(File::canRead)
                            .map(f -> new Import(null, f.getName().replace(Constants.LKQL_EXTENSION, ""), -1))
                            .toList()
                    );
                }
            }
        }

        // Set the rule imports children
        this.ruleImports = ruleImports.toArray(new Import[0]);
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
