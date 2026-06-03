//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.adacore.lkql_jit.values.streams.LKQLSelectorList;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public final class SelectorExpr extends Expr {

    // ----- Attributes -----

    /** Root node for the lazy execution of the selector. */
    private FunctionRootNode rootNode;

    public final String name;

    // ----- Constructors -----

    /**
     * Create a new body node for a selector.
     * This expression returns a stream containing the result of the selector call.
     */
    public SelectorExpr(
        SourceSection location,
        FrameDescriptor frameDescriptor,
        Expr body,
        String name,
        boolean isMemoized
    ) {
        super(location);
        this.rootNode = new FunctionRootNode(
            LKQLLanguage.getLanguage(this),
            frameDescriptor,
            isMemoized,
            true,
            new String[] { Constants.THIS_SYMBOL },
            new Expr[] { null },
            body,
            name
        );
        this.name = name;
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeStream(frame);
    }

    @Override
    public LKQLStream executeStream(VirtualFrame frame) {
        var args = frame.getArguments();
        var closure = args[0] instanceof Closure c ? c : Closure.EMPTY; // closure is shared with selector decl
        var root = args[1];
        var depth = (long) args[2];
        var minDepth = (long) args[3];
        var maxDepth = (long) args[4];
        return new LKQLSelectorList(rootNode, closure, root, depth, maxDepth, minDepth);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel, new String[] {}, new Object[] {});
    }
}
