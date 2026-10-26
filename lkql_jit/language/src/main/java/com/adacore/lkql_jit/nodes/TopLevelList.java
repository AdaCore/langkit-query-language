//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.values.LKQLNamespace;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Set;

/**
 * This node represents the list of all top level instructions of a LKQL program. It's the "highest"
 * node in a LKQL AST and is the starting point of the program.
 */
public final class TopLevelList extends LKQLNode {

    // ----- Attributes -----

    /** Descriptor of the top level frame. */
    private final FrameDescriptor frameDescriptor;

    /** Array of symbols that the module represented by this top level list exports. */
    private final Set<String> exportedSymbols;

    /** Documentation for the toplevel list */
    private final String doc;

    // ----- Children -----

    /** The list of nodes representing the LKQL program. */
    @Children
    public final LKQLNode[] program;

    private final boolean isInteractive;

    // ----- Constructors -----

    /**
     * Create a new top level list node.
     *
     * @param location The location of the node in the source.
     * @param frameDescriptor The frame descriptor for the top level.
     * @param exportedSymbols Array of symbols exported by this top level.
     * @param nodes The nodes to execute in the top level.
     * @param isInteractive Whether the node is for an interactive context.
     * @param doc The documentation of the module defined by this node.
     */
    public TopLevelList(
        SourceSection location,
        FrameDescriptor frameDescriptor,
        Set<String> exportedSymbols,
        LKQLNode[] nodes,
        boolean isInteractive,
        String doc
    ) {
        super(location);
        this.frameDescriptor = frameDescriptor;
        this.exportedSymbols = exportedSymbols;
        this.program = nodes;
        this.isInteractive = isInteractive;
        this.doc = doc;
    }

    // ----- Getters -----

    public FrameDescriptor getFrameDescriptor() {
        return this.frameDescriptor;
    }

    // ----- Execution methods -----

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        CompilerAsserts.compilationConstant(this.program.length);

        Object val = null;

        // Execute the nodes of the program
        for (int i = 0; i < program.length; i++) {
            val = program[i].executeGeneric(frame);
        }

        // Get the language context and initialize it
        final LKQLContext context = LKQLContext.get(this);

        // Create the module namespace
        var namespace = LKQLNamespace.createUncached(
            frame.materialize(),
            exportedSymbols,
            doc,
            this.location
        );

        if (this.isInteractive) {
            // In interactive mode, return the last evaluated value, and add the namespace values
            // to the global namespace
            this.updateGlobals(namespace);
            return context.getEnv().asGuestValue(val);
        } else {
            // Else return the namespace corresponding to the program execution
            return namespace;
        }
    }

    @CompilerDirectives.TruffleBoundary
    private void updateGlobals(LKQLNamespace namespace) {
        var context = LKQLContext.get(this);
        var globalObjects = context.getGlobal().getGlobalObjects();
        globalObjects.putAll(namespace.asMap());
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
