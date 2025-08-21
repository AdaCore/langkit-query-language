//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations.selector;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.declarations.Annotation;
import com.adacore.lkql_jit.nodes.declarations.Declaration;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.root_nodes.SelectorRootNode;
import com.adacore.lkql_jit.nodes.utils.CreateClosureNode;
import com.adacore.lkql_jit.runtime.values.LKQLSelector;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.CompilerDirectives;
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
    @CompilerDirectives.CompilationFinal
    private final int slot;

    /** The closure descriptor of the selector. */
    @CompilerDirectives.CompilationFinal
    private final ClosureDescriptor closureDescriptor;

    /** The root node of the selector. */
    private final SelectorRootNode selectorRootNode;

    @Child
    CreateClosureNode createClosureNode;

    // ----- Constructors -----

    /**
     * Create a new selector declaration node.
     */
    public SelectorDeclaration(
        SourceSection location,
        Annotation annotation,
        FrameDescriptor frameDescriptor,
        ClosureDescriptor closureDescriptor,
        String name,
        String documentation,
        int slot,
        Expr body
    ) {
        super(location, annotation);
        this.closureDescriptor = closureDescriptor;
        this.name = name;
        this.documentation = documentation;
        this.slot = slot;

        this.selectorRootNode = new SelectorRootNode(
            LKQLLanguage.getLanguage(this),
            frameDescriptor,
            annotation != null && annotation.getName().equals(Constants.ANNOTATION_MEMOIZED),
            body,
            name
        );
        this.createClosureNode = new CreateClosureNode(closureDescriptor);
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        FrameUtils.writeLocal(
            frame,
            this.slot,
            new LKQLSelector(
                this.selectorRootNode,
                createClosureNode.execute(frame),
                this.name,
                this.documentation,
                // We only check cycles on memoized selectors for now
                annotation != null && annotation.getName().equals(Constants.ANNOTATION_MEMOIZED)
            )
        );
        return LKQLUnit.INSTANCE;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "name", "slot" },
                new Object[] { this.name, this.slot }
            );
    }
}
