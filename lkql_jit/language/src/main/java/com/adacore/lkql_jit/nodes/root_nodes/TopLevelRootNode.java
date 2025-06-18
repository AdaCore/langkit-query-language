//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This root node represents the root execution of an LKQL program.
 *
 * @author Hugo GUERRIER
 */
public final class TopLevelRootNode extends BaseRootNode {

    // ----- Attributes -----

    /** Whether this top level root node comes from an import request. */
    private final boolean fromImport;

    /** The list of nodes representing the program. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private TopLevelList program;

    // ----- Constructors -----

    /**
     * Create a new LKQL top level root node.
     *
     * @param fromImport Whether the node has been created from an import statement.
     * @param program The LKQL program to execute.
     * @param language The reference to the LKQL language instance.
     */
    public TopLevelRootNode(
        final boolean fromImport,
        final TopLevelList program,
        final LKQLLanguage language
    ) {
        super(language, program.getFrameDescriptor());
        this.fromImport = fromImport;
        this.program = program;
    }

    // ----- Execution methods -----

    /**
     * Execute the LKQL program and return the namespace, result of this program execution.
     */
    @Override
    public Object execute(VirtualFrame frame) {
        // Initialize the frame
        this.initFrame(frame);

        // Execute the program
        return this.program.executeGeneric(frame);
    }

    @Override
    public String toString() {
        return "<" + program.getSourceSection().getSource().getName() + ">";
    }

    @Override
    public String getName() {
        return program.getSourceSection().getSource().getName();
    }
}
