//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.TopLevelList;
import com.adacore.lkql_jit.utils.functions.FileUtils;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This root node represents the root execution of an LKQL program.
 *
 * @author Hugo GUERRIER
 */
public final class TopLevelRootNode extends BaseRootNode {

    // ----- Attributes -----

    /** The list of nodes representing the program. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private TopLevelList program;

    // ----- Constructors -----

    /**
     * Create a new LKQL top level root node.
     *
     * @param program The LKQL program to execute.
     * @param language The reference to the LKQL language instance.
     */
    public TopLevelRootNode(final TopLevelList program, final LKQLLanguage language) {
        super(program.getSourceSection(), language, program.getFrameDescriptor());
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

        // Push the source corresponding to this top level list on the execution stack
        var context = LKQLLanguage.getContext(this.program);
        context.pushSourceToStack(FileUtils.sourcePathOrName(getSourceSection().getSource()));

        // Execute the program and in all cases, pop the source from the stack
        try {
            return this.program.executeGeneric(frame);
        } finally {
            context.popSourceFromStack();
        }
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
