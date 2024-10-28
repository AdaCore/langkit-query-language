//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.options.LKQLOptions;
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
            final boolean fromImport, final TopLevelList program, final LKQLLanguage language) {
        super(language, program.getFrameDescriptor());
        this.fromImport = fromImport;
        this.program = program;
    }

    // ----- Execution methods -----

    /**
     * Execute the LKQL program and return the namespace, result of this program execution.
     *
     * @param frame The frame to execute in.
     * @return The namespace of the LKQL program.
     * @see com.oracle.truffle.api.nodes.RootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object execute(VirtualFrame frame) {
        // If the checker mode is activated add all rule imports
        final var engineMode = LKQLLanguage.getContext(this.program).getEngineMode();
        if (!fromImport
                && (engineMode == LKQLOptions.EngineMode.CHECKER
                        || engineMode == LKQLOptions.EngineMode.FIXER)) {
            this.program.addRuleImports();
        }

        // Initialize the frame
        this.initFrame(frame);

        // Execute the program
        return this.program.executeGeneric(frame);
    }
}
