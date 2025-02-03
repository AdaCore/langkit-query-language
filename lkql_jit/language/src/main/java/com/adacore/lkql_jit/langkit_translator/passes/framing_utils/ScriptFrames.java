//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes.framing_utils;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.exception.TranslatorException;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class represents the description of the frames of an LKQL script.
 *
 * @author Hugo GUERRIER
 */
public final class ScriptFrames {

    // ----- Attributes -----

    /** LKQL built-ins with their slot. */
    private final Map<String, Integer> builtIns;

    /** Root node frame. */
    private final AbstractNodeFrame root;

    /** Pointer to the current node frame. */
    private AbstractNodeFrame current;

    private final GlobalScope globalScope;

    // ----- Constructors -----

    /**
     * Create a new script frames description.
     *
     * @param builtIns The built-in symbols.
     * @param root The root node frame of the script frames description.
     * @param globalScope
     */
    public ScriptFrames(
        final List<String> builtIns,
        final AbstractNodeFrame root,
        GlobalScope globalScope
    ) {
        this.globalScope = globalScope;
        this.builtIns = new HashMap<>();
        for (int i = 0; i < builtIns.size(); i++) {
            this.builtIns.put(builtIns.get(i), i);
        }
        this.root = root;
        this.current = null;
    }

    // ----- Instance methods -----

    // --- Frame methods

    /**
     * Enter the frame associated to the given node.
     *
     * @param node The node associated to the frame to enter in.
     */
    public void enterFrame(final Liblkqllang.LkqlNode node) {
        if (this.current == null) {
            if (!this.root.node.equals(node)) {
                throw new TranslatorException(
                    "Cannot enter the root frame because of node inequality"
                );
            }
            this.current = this.root;
        } else {
            if (!this.current.children.containsKey(node)) {
                throw new TranslatorException(
                    "Cannot enter the frame, " +
                    node +
                    " isn't in the children" +
                    " (current: " +
                    this.current.node +
                    ")"
                );
            }
            this.current = this.current.children.get(node);
        }
    }

    /** Exit the current frame and go back to its parent */
    public void exitFrame() {
        if (this.current.parent != null) {
            this.current = this.current.parent;
        }
    }

    /**
     * Get the Truffle frame descriptor of the current frame.
     *
     * @return The Truffle frame descriptor.
     */
    public FrameDescriptor getFrameDescriptor() {
        if (this.current instanceof VirtualNodeFrame) {
            throw new TranslatorException("Cannot get frame descriptor for a virtual frame");
        }
        return ((NodeFrame) this.current).frameDescriptorBuilder.build();
    }

    /**
     * Get the closure descriptor of the current frame.
     *
     * @return The closure descriptor.
     */
    public ClosureDescriptor getClosureDescriptor() {
        if (this.current instanceof VirtualNodeFrame) {
            throw new TranslatorException("Cannot get closure descriptor for a virtual frame");
        }
        return ((NodeFrame) this.current).getClosureDescriptor();
    }

    public boolean isPrelude(final String symbol) {
        return globalScope.preludeMap.containsKey(symbol);
    }

    public int getPrelude(final String symbol) {
        return globalScope.preludeMap.get(symbol);
    }

    // --- Symbol methods

    /**
     * Get if the given symbol is a built-in
     *
     * @param symbol The symbol to look for.
     * @return True if the symbol is in the built-in, false else.
     */
    public boolean isBuiltIn(final String symbol) {
        return this.builtIns.containsKey(symbol);
    }

    /**
     * Get the built-in slot for the given built-in symbol.
     *
     * @param symbol The symbol to get the slot for.
     * @return The built-in slot.
     */
    public int getBuiltIn(final String symbol) {
        return this.builtIns.get(symbol);
    }

    /**
     * Get if the given symbol is in the current frame bindings.
     *
     * @param symbol The symbol to look for.
     * @return True if the symbol is in the bindings, false else.
     */
    public boolean isBinding(final String symbol) {
        return this.current.getBinding(symbol).slot > -1;
    }

    /**
     * Get if the given binding symbol was declared at this stage.
     *
     * @param symbol The symbol to get the declaration state for.
     * @return True if the symbol has been declared, false else.
     */
    public boolean isBindingDeclared(final String symbol) {
        return this.current.getBinding(symbol).declared;
    }

    /**
     * Get the binding slot of the given symbol.
     *
     * @param symbol The symbol to get the slot of.
     * @return The binding slot.
     */
    public int getBinding(final String symbol) {
        return this.current.getBinding(symbol).slot;
    }

    /**
     * Flag the given symbol as declared in the current bindings.
     *
     * @param symbol The symbol to flag as declared.
     */
    public void declareBinding(final String symbol) {
        this.current.declareBinding(symbol);
    }

    /**
     * Get if the given symbol is in the parameters of the current frame.
     *
     * @param symbol The symbol to look for.
     * @return True if the symbol is in the parameters, false else.
     */
    public boolean isParameter(final String symbol) {
        return this.current.getParameter(symbol) > -1;
    }

    /**
     * Get the parameter slot of the given symbol.
     *
     * @param symbol The symbol to get the slot of.
     * @return The parameter slot.
     */
    public int getParameter(final String symbol) {
        return this.current.getParameter(symbol);
    }

    /**
     * Get if the symbol is accessible in the closure of the current frame.
     *
     * @param symbol The symbol to look for.
     * @return True of the symbol is accessible in the closure, false else.
     */
    public boolean isClosure(final String symbol) {
        return this.current.getClosure(symbol) > -1;
    }

    /**
     * Get if the symbol is declared in the closure of the current frame.
     *
     * @param symbol The symbol to look for.
     * @return True if the symbol is declared in the closure, false else.
     */
    public boolean isClosureDeclared(final String symbol) {
        return this.current.isClosureDeclared(symbol);
    }

    /**
     * Get the closure slot of the given symbol.
     *
     * @param symbol The symbol to get the slot of.
     * @return The closure slot.
     */
    public int getClosure(final String symbol) {
        return this.current.getClosure(symbol);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return (
            "ScriptFrames(" + "\n\tbuilt_ins: " + this.builtIns + "\n\troot: " + this.root + "\n)"
        );
    }

    // ----- Inner classes -----

    /** This abstract class represents the base of all node frame representations. */
    public abstract static class AbstractNodeFrame {

        // ----- Records -----

        /** Pseudo mutable record to store information about a binding. */
        public static final class BindingInfo {

            private static final BindingInfo NONE = new BindingInfo(-1);

            private final int slot;
            private boolean declared;

            private BindingInfo(int slot) {
                this.slot = slot;
                this.declared = false;
            }

            @Override
            public String toString() {
                return String.valueOf(this.slot);
            }
        }

        // ----- Attributes -----

        // --- General attributes

        /** The node associated with the frame. */
        protected final Liblkqllang.LkqlNode node;

        /** The parent node frame. */
        protected final AbstractNodeFrame parent;

        /** List of the children node frames. */
        protected final Map<Liblkqllang.LkqlNode, AbstractNodeFrame> children;

        /** Bindings in the frame, those are local variables declared in the frame. */
        protected final Map<String, BindingInfo> bindings;

        // ----- Constructors -----

        /**
         * Create a new node frame with its associated node and its parent.
         *
         * @param node Associated node.
         * @param parent The parent of the node frame.
         */
        protected AbstractNodeFrame(
            final Liblkqllang.LkqlNode node,
            final AbstractNodeFrame parent
        ) {
            this.node = node;
            this.parent = parent;
            this.children = new HashMap<>();
            this.bindings = new HashMap<>();
        }

        // ----- Instance methods -----

        // --- Symbol methods

        /**
         * Get the binding slot of the given symbol.
         *
         * @param symbol The symbol to get the slot of.
         * @return The binding slot or -1 if the bindings doesn't exist.
         */
        public abstract BindingInfo getBinding(String symbol);

        /**
         * Add the given symbol to the frame bindings.
         *
         * @param symbol The symbol to add.
         */
        public abstract void addBinding(String symbol);

        /**
         * Flag the given symbol as declared in the bindings map.
         *
         * @param symbol The symbol to flag as declared.
         */
        public void declareBinding(final String symbol) {
            if (!this.bindings.containsKey(symbol)) {
                throw new TranslatorException(
                    "Cannot declare the binding '" + symbol + "' if it doesn't exist"
                );
            }
            this.bindings.get(symbol).declared = true;
        }

        /**
         * Get the parameter slot of the given symbol.
         *
         * @param symbol The symbol to get the slot of.
         * @return The parameter slot or -1 if the parameter doesn't exist.
         */
        public abstract int getParameter(String symbol);

        /**
         * Add the given symbol to the frame parameters.
         *
         * @param symbol The symbol to add.
         */
        public abstract void addParameter(String symbol);

        /**
         * Get if the given symbol is declared in its real frame. This method assume that the given
         * symbol is already in the frame closure.
         *
         * @param symbol The symbol to look for.
         * @return True if the symbol is declared in its real frame, false else.
         */
        public abstract boolean isClosureDeclared(String symbol);

        /**
         * Get the closure slot for the given symbol. Modify the closure descriptions by getting the
         * symbol.
         *
         * @param symbol The symbol to get the slot of.
         * @return The closure slot or -1 if the symbol doesn't exist in the closure.
         */
        public abstract int getClosure(String symbol);

        // --- Children methods

        /**
         * Add the given child to the children list.
         *
         * @param child The child to add.
         */
        public void addChild(final AbstractNodeFrame child) {
            this.children.put(child.node, child);
        }

        /**
         * Get an available slot for the given child.
         *
         * @param child The child to get the slot for.
         * @return An available slot in the frame.
         */
        protected abstract int getSlotForChild(AbstractNodeFrame child);
    }

    /** This class represent a frame associated with a node. */
    public static final class NodeFrame extends AbstractNodeFrame {

        // ----- Attributes -----

        /** Parameters in the frame, those are parameters passed to the frame. */
        private final Map<String, Integer> parameters;

        /** Closure of the frame, symbols imported from parent frames. */
        private final Map<String, Integer> closure;

        /** Bindings of the parent to close in this node frame. */
        private final Map<Integer, Integer> closingBindings;

        /** Parameters of the parent to close in this node frame. */
        private final Map<Integer, Integer> closingParameters;

        /** Closure elements of the parent to close in this node frame. */
        private final Map<Integer, Integer> closingClosure;

        /**
         * Frame descriptor builder to describe the frame to Truffle. If this is null, the frame is
         * virtual.
         */
        private final FrameDescriptor.Builder frameDescriptorBuilder;

        /** Allocated slots for the virtual children. */
        private final List<Integer> virtualChildrenSlots;

        /** A map to store which virtual children is using which slots. */
        private final Map<AbstractNodeFrame, List<Integer>> virtualChildrenAssociatedSlots;

        /** The counter for the parameter slots. */
        private int parameterCounter;

        /** The counter for the closure slots. */
        private int closureCounter;

        // ----- Constructors -----

        /**
         * Create a new frame description associated to a node.
         *
         * @param node The node associated with the frame description.
         * @param parent The parent node frame description, this can be null.
         */
        public NodeFrame(final Liblkqllang.LkqlNode node, final AbstractNodeFrame parent) {
            super(node, parent);
            this.parameters = new HashMap<>();
            this.closure = new HashMap<>();
            this.closingBindings = new HashMap<>();
            this.closingParameters = new HashMap<>();
            this.closingClosure = new HashMap<>();
            this.frameDescriptorBuilder = FrameDescriptor.newBuilder();
            this.virtualChildrenSlots = new ArrayList<>();
            this.virtualChildrenAssociatedSlots = new HashMap<>();
            this.parameterCounter = 1;
            this.closureCounter = 0;
        }

        // ----- Instance methods -----

        // --- Frame methods

        /**
         * Create and return a closure descriptor of the frame.
         *
         * @return The closure descriptor.
         */
        public ClosureDescriptor getClosureDescriptor() {
            return new ClosureDescriptor(
                this.closureCounter,
                this.closingBindings,
                this.closingParameters,
                this.closingClosure
            );
        }

        // --- Symbol methods

        @Override
        public BindingInfo getBinding(final String symbol) {
            return this.bindings.getOrDefault(symbol, BindingInfo.NONE);
        }

        @Override
        public void addBinding(final String symbol) {
            // Get a new slot in the frame descriptor builder
            final int slot;
            if (this.node instanceof Liblkqllang.TopLevelList) {
                slot = this.frameDescriptorBuilder.addSlot(FrameSlotKind.Object, symbol, null);
            } else {
                slot = this.frameDescriptorBuilder.addSlot(FrameSlotKind.Object, null, null);
            }

            // Put the slot with the name in the bindings map
            this.bindings.put(symbol, new BindingInfo(slot));
        }

        @Override
        public int getParameter(String symbol) {
            return this.parameters.getOrDefault(symbol, -1);
        }

        @Override
        public void addParameter(final String symbol) {
            final int slot = this.parameterCounter++;
            this.parameters.put(symbol, slot);
        }

        @Override
        public boolean isClosureDeclared(String symbol) {
            // Ensure the symbol exists in the closure
            if (!this.closure.containsKey(symbol)) {
                throw new TranslatorException(
                    "Cannot verify declaration of a non-existing closure symbol: '" + symbol + "'"
                );
            }

            // Look in the parent if it is not null
            final int slot = this.closure.get(symbol);
            if (this.parent != null) {
                // Look in the parent bindings
                if (this.closingBindings.containsKey(slot)) {
                    return this.parent.getBinding(symbol).declared;
                }
                // Look in the parent parameters
                else if (this.closingParameters.containsKey(slot)) {
                    return true;
                }
                // Else look in the parent closure
                else {
                    return this.parent.isClosureDeclared(symbol);
                }
            }

            // Return the default result
            return false;
        }

        @Override
        public int getClosure(final String symbol) {
            // Look in the already existing closure symbols
            if (this.closure.containsKey(symbol)) {
                return this.closure.get(symbol);
            }

            // Look in the parent if it is not null
            if (this.parent != null) {
                // Look in the parent bindings
                final int bindingSlot = this.parent.getBinding(symbol).slot;
                if (bindingSlot > -1) {
                    final int slot = this.closureCounter++;
                    this.closure.put(symbol, slot);
                    this.closingBindings.put(slot, bindingSlot);
                    return slot;
                }

                // Look in the parent parameters
                final int parameterSlot = this.parent.getParameter(symbol);
                if (parameterSlot > -1) {
                    final int slot = this.closureCounter++;
                    this.closure.put(symbol, slot);
                    this.closingParameters.put(slot, parameterSlot);
                    return slot;
                }

                // Look in the parent closure
                final int closureSlot = this.parent.getClosure(symbol);
                if (closureSlot > -1) {
                    final int slot = this.closureCounter++;
                    this.closure.put(symbol, slot);
                    this.closingClosure.put(slot, closureSlot);
                    return slot;
                }
            }

            // Return -1 by default
            return -1;
        }

        // --- Children methods

        @Override
        protected int getSlotForChild(final AbstractNodeFrame child) {
            // If the child hasn't any associated slots list, add an empty one
            if (!this.virtualChildrenAssociatedSlots.containsKey(child)) {
                this.virtualChildrenAssociatedSlots.put(child, new ArrayList<>());
            }

            // Get the next available slot for the child and return it
            final List<Integer> childAssociatedSlots =
                this.virtualChildrenAssociatedSlots.get(child);
            if (childAssociatedSlots.size() >= this.virtualChildrenSlots.size()) {
                this.virtualChildrenSlots.add(
                        this.frameDescriptorBuilder.addSlot(FrameSlotKind.Object, null, null)
                    );
            }
            final int slot = this.virtualChildrenSlots.get(childAssociatedSlots.size());
            childAssociatedSlots.add(slot);
            return slot;
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return (
                "NodeFrame(" +
                "node: " +
                this.node +
                (!this.bindings.isEmpty() ? ", bindings: " + this.bindings : "") +
                (!this.parameters.isEmpty() ? ", parameters: " + this.parameters : "") +
                (!this.closure.isEmpty() ? ", closure: " + this.closure : "") +
                (!this.closingBindings.isEmpty()
                        ? ", closing_bindings: " + this.closingBindings
                        : "") +
                (!this.closingParameters.isEmpty()
                        ? ", closing_parameters: " + this.closingParameters
                        : "") +
                (!this.closingClosure.isEmpty()
                        ? ", closing_closure: " + this.closingClosure
                        : "") +
                (!this.children.isEmpty() ? ", children: " + this.children.values() : "") +
                ")"
            );
        }
    }

    /** This class represents a virtual node frame. */
    public static final class VirtualNodeFrame extends AbstractNodeFrame {

        // ----- Constructors -----

        /**
         * Create a new virtual node frame with its node and parent.
         *
         * @param node The node associated to the frame.
         * @param parent The node frame parent.
         */
        public VirtualNodeFrame(final Liblkqllang.LkqlNode node, final AbstractNodeFrame parent) {
            super(node, parent);
        }

        // ----- Instance methods -----

        // --- Symbol methods

        @Override
        public BindingInfo getBinding(String symbol) {
            if (this.bindings.containsKey(symbol)) {
                return this.bindings.get(symbol);
            } else {
                return this.parent.getBinding(symbol);
            }
        }

        @Override
        public void addBinding(final String symbol) {
            this.bindings.put(symbol, new BindingInfo(this.parent.getSlotForChild(this)));
        }

        @Override
        public int getParameter(String symbol) {
            return this.parent.getParameter(symbol);
        }

        @Override
        public void addParameter(final String symbol) {
            throw new TranslatorException("Cannot add a parameter in a virtual frame");
        }

        @Override
        public boolean isClosureDeclared(String symbol) {
            return this.parent.isClosureDeclared(symbol);
        }

        @Override
        public int getClosure(String symbol) {
            return this.parent.getClosure(symbol);
        }

        // --- Children methods

        @Override
        protected int getSlotForChild(AbstractNodeFrame child) {
            return this.parent.getSlotForChild(this);
        }

        // ----- Override methods -----

        @Override
        public String toString() {
            return (
                "VirtualNodeFrame(" +
                "node: " +
                this.node +
                (!this.bindings.isEmpty() ? ", bindings: " + this.bindings : "") +
                (!this.children.isEmpty() ? ", children: " + this.children.values() : "") +
                ")"
            );
        }
    }
}
