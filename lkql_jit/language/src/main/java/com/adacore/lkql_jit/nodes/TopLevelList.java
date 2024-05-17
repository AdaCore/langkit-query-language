//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes;

import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.LKQLNamespace;
import com.adacore.lkql_jit.nodes.declarations.Import;
import com.adacore.lkql_jit.utils.Constants;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * This node represents the list of all top level instructions of a LKQL program. It's the "highest"
 * node in a LKQL AST and is the starting point of the program.
 *
 * @author Hugo GUERRIER
 */
public final class TopLevelList extends LKQLNode {

    // ----- Attributes -----

    /** Descriptor of the top level frame. */
    private final FrameDescriptor frameDescriptor;

    /** Documentation for the toplevel list */
    private final String doc;

    // ----- Children -----

    /** The rule importation nodes. */
    @Children private Import[] ruleImports;

    /** The list of nodes representing the LKQL program. */
    @Children private final LKQLNode[] program;

    private final boolean isInteractive;

    // ----- Constructors -----

    /**
     * Create a new top level list node.
     *
     * @param location The location of the node in the source.
     * @param frameDescriptor The frame descriptor for the top level.
     * @param nodes The nodes to execute in the top level.
     */
    public TopLevelList(
            SourceSection location,
            FrameDescriptor frameDescriptor,
            LKQLNode[] nodes,
            boolean isInteractive,
            String doc) {
        super(location);
        this.frameDescriptor = frameDescriptor;
        this.program = nodes;
        this.isInteractive = isInteractive;
        this.doc = doc;
    }

    // ----- Getters -----

    public FrameDescriptor getFrameDescriptor() {
        return this.frameDescriptor;
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // If there is rule imports, run them
        if (this.ruleImports != null) {
            for (Import ruleImport : this.ruleImports) {
                ruleImport.executeGeneric(frame);
            }
        }

        Object val = null;

        // Execute the nodes of the program
        for (LKQLNode lkqlNode : program) {
            val = lkqlNode.executeGeneric(frame);
        }

        // Get the language context and initialize it
        final LKQLContext context = LKQLLanguage.getContext(this);

        if (this.isInteractive) {
            // In interactive mode, return the last evaluated value, and add the namespace values
            // to the global namespace
            this.updateGlobals(LKQLNamespace.createUncached(frame.materialize(), doc));
            return context.getEnv().asGuestValue(val);
        } else {
            // Else return the namespace corresponding to the program execution
            return LKQLNamespace.createUncached(frame.materialize(), doc);
        }
    }

    @CompilerDirectives.TruffleBoundary
    private void updateGlobals(LKQLNamespace namespace) {
        var context = LKQLLanguage.getContext(this);
        var globalObjects = context.getGlobal().getGlobalObjects();
        globalObjects.putAll(namespace.asMap());
    }

    // ----- Class methods -----

    /** Add all required rule importing nodes. */
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
                final File[] ruleDirectoryFiles =
                        ruleDirectory.listFiles(
                                f -> f.canRead() && f.getName().endsWith(Constants.LKQL_EXTENSION));
                if (ruleDirectoryFiles != null) {
                    ruleImports.addAll(
                            Arrays.stream(ruleDirectoryFiles)
                                    .filter(File::canRead)
                                    .map(
                                            f ->
                                                    new Import(
                                                            null,
                                                            f.getName()
                                                                    .replace(
                                                                            Constants
                                                                                    .LKQL_EXTENSION,
                                                                            ""),
                                                            -1))
                                    .toList());
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
