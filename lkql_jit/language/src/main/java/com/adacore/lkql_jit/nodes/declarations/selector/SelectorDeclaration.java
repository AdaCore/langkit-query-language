//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations.selector;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.LKQLSelector;
import com.adacore.lkql_jit.built_ins.values.LKQLUnit;
import com.adacore.lkql_jit.nodes.declarations.Annotation;
import com.adacore.lkql_jit.nodes.declarations.Declaration;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the base declaration of a selector in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class SelectorDeclaration extends Declaration {

    // ----- Attributes -----

    /** The name of the selector. */
    private final String name;

    /** The documentation of the selector. */
    private final String documentation;

    /** The slot to put the selector in. */
    private final int slot;

    /** The closure descriptor of the selector. */
    private final ClosureDescriptor closureDescriptor;

    /** The root node of the selector. */
    private final SelectorRootNode selectorRootNode;

    // ----- Constructors -----

    /**
     * Create a new selector declaration node.
     *
     * @param location The location of the node in the source.
     * @param annotation The annotation of the selector declaration.
     * @param frameDescriptor The frame descriptor for the selector.
     * @param closureDescriptor The closure descriptor for the selector root node.
     * @param name The name of the selector.
     * @param documentation The documentation of the selector.
     * @param slot The slot to put the selector in.
     * @param thisSlot The slot for the "this" symbol.
     * @param depthSlot The slot for the "depth" symbol.
     * @param arms The arms of the selector.
     */
    public SelectorDeclaration(
            SourceSection location,
            Annotation annotation,
            FrameDescriptor frameDescriptor,
            ClosureDescriptor closureDescriptor,
            String name,
            String documentation,
            int slot,
            int thisSlot,
            int depthSlot,
            SelectorArm[] arms) {
        super(location, annotation);
        this.closureDescriptor = closureDescriptor;
        this.name = name;
        this.documentation = documentation;
        this.slot = slot;

        this.selectorRootNode =
                new SelectorRootNode(
                        LKQLLanguage.getLanguage(this),
                        frameDescriptor,
                        annotation != null
                                && annotation.getName().equals(Constants.ANNOTATION_MEMOIZED),
                        thisSlot,
                        depthSlot,
                        arms);
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        FrameUtils.writeLocal(
                frame,
                this.slot,
                new LKQLSelector(
                        this.selectorRootNode,
                        Closure.create(frame.materialize(), this.closureDescriptor),
                        this.name,
                        this.documentation));
        return LKQLUnit.INSTANCE;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"name", "slot"}, new Object[] {this.name, this.slot});
    }
}
