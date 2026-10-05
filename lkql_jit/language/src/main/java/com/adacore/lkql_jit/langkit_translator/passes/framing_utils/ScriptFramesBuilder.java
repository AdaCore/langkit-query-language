//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes.framing_utils;

import com.adacore.langkit_support.LangkitSupport.NodeInterface;
import com.adacore.lkql_jit.built_ins.AllBuiltIns;
import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.runtime.GlobalScope;
import java.util.ArrayList;
import java.util.List;

/** Builder class to construct frames for an LKQL script. */
public final class ScriptFramesBuilder {

    // ----- Attributes -----

    /** Root node frame builder. This is where to start the building. */
    private NodeFrameBuilder root;

    /** Pointer to the current node frame builder. */
    private NodeFrameBuilder current;

    // ----- Constructors -----

    /** Create a new script frames builder with the script root node. */
    public ScriptFramesBuilder() {
        this.root = null;
        this.current = null;
    }

    // ----- Instance methods -----

    // --- Frame methods

    /**
     * Open a new non-virtual frame builder with its associated node.
     *
     * @param node The node that opens the frame.
     */
    public void openFrame(final NodeInterface node) {
        openFrame(node, false);
    }

    /**
     * Open a new virtual frame builder with its associated node.
     *
     * @param node The node that opens the frame.
     */
    public void openVirtualFrame(final NodeInterface node) {
        openFrame(node, true);
    }

    /** Internal function to factorize frame builder opening. */
    private void openFrame(final NodeInterface node, final boolean isVirtual) {
        // Create the new frame
        final NodeFrameBuilder newFrame = new NodeFrameBuilder(node, this.current, isVirtual);

        // If the frame is the first to be opened set it as the root
        if (this.root == null) {
            if (isVirtual) {
                throw LKQLEngineException.create("Cannot open a virtual frame as a root frame");
            }
            this.root = newFrame;
        }
        // Else, add it as a child of the current frame
        else {
            this.current.children.add(newFrame);
        }

        // Set the current frame to the newly created one
        this.current = newFrame;
    }

    /**
     * Close the current frame and move to its parent. You cannot close the root node frame builder.
     */
    public void closeFrame() {
        if (this.current.parent != null) {
            this.current = this.current.parent;
        }
    }

    // --- Symbol methods

    /** Get whether the current frame contains the given symbol in its bindings. */
    public boolean bindingExists(final String symbol) {
        if (this.current.parent == null) {
            return (
                this.current.bindings.contains(symbol) ||
                AllBuiltIns.functions().containsKey(symbol)
            );
        } else {
            return this.current.bindings.contains(symbol);
        }
    }

    /** Add the given symbol to the current bindings. */
    public void addBinding(final String symbol) {
        this.current.bindings.add(symbol);
    }

    /** Get whether parameters in the current frame contains the given symbol. */
    public boolean parameterExists(final String symbol) {
        return this.current.parameters.contains(symbol);
    }

    /** Add the given symbol to the current parameters. */
    public void addParameter(final String symbol) {
        this.current.parameters.add(symbol);
    }

    // --- Building methods

    /** Build script frames described by this builder and return them. */
    public ScriptFrames build(GlobalScope globalScope) {
        if (this.current != this.root) {
            throw LKQLEngineException.create("Framing pass didn't close all opened frames");
        }

        // If no frame has been opened return an empty frame
        final ScriptFrames.AbstractNodeFrame rootNodeFrame;
        if (this.root == null) {
            rootNodeFrame = new ScriptFrames.NodeFrame(null, null);
        } else {
            rootNodeFrame = this.root.build(null);
        }
        return new ScriptFrames(rootNodeFrame, globalScope);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return "ScriptFramesBuilder(" + ", root: " + this.root + ")";
    }

    // ----- Inner classes -----

    /** This class represents a frame builder associated to a node. */
    public static final class NodeFrameBuilder {

        // ----- Attributes -----

        /** Syntactical node associated with the frame builder. */
        private final NodeInterface node;

        /** Parent node frame builder. */
        private final NodeFrameBuilder parent;

        /** Children node frame builders. */
        private final List<NodeFrameBuilder> children;

        /** Bindings in the current frame. */
        private final List<String> bindings;

        /** Parameters of the current frame. */
        private final List<String> parameters;

        /**
         * If the frame is virtual or not. A virtual frame isn't associated to a Truffle root node
         * and is store in the nearest non-virtual parent frame.
         */
        private final boolean isVirtual;

        // ----- Constructors -----

        /**
         * Create a new node frame builder with all required information.
         *
         * @param node The node that opens the frame.
         * @param parent Parent frame builder, can be null.
         * @param isVirtual Whether the frame is a virtual one.
         */
        private NodeFrameBuilder(
            final NodeInterface node,
            final NodeFrameBuilder parent,
            final boolean isVirtual
        ) {
            this.node = node;
            this.parent = parent;
            this.children = new ArrayList<>();
            this.bindings = new ArrayList<>();
            this.parameters = new ArrayList<>();
            this.isVirtual = isVirtual;
        }

        // ----- Instance methods -----

        /** Build the frame described by this builder and return it. */
        public ScriptFrames.AbstractNodeFrame build(final ScriptFrames.AbstractNodeFrame parent) {
            // Create the result node frame
            final ScriptFrames.AbstractNodeFrame res = this.isVirtual
                ? new ScriptFrames.VirtualNodeFrame(this.node, parent)
                : new ScriptFrames.NodeFrame(this.node, parent);

            // Add all bindings to the node frame
            for (String binding : this.bindings) {
                res.addBinding(binding);
            }

            // Add all parameters to the node frame
            for (String parameter : this.parameters) {
                res.addParameter(parameter);
            }

            // Build all children
            for (NodeFrameBuilder child : this.children) {
                res.addChild(child.build(res));
            }

            // Return the node frame
            return res;
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return (
                "NodeFrameBuilder" +
                (this.isVirtual ? "<virtual>" : "") +
                "(" +
                "node: " +
                node.getImage() +
                (this.bindings.size() > 0 ? ", bindings: " + this.bindings : "") +
                (this.parameters.size() > 0 ? ", parameters: " + this.parameters : "") +
                (this.children.size() > 0 ? ", children: " + this.children : "") +
                ")"
            );
        }
    }
}
