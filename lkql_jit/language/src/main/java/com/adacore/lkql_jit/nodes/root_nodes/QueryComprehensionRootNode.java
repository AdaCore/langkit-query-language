//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBoolean;
import com.adacore.lkql_jit.nodes.expressions.LKQLToBooleanNodeGen;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

public final class QueryComprehensionRootNode extends BaseRootNode {

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Pattern pattern;

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr guard;

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr result;

    @Child
    private LKQLToBoolean toBoolean;

    // ----- Constructors -----

    public QueryComprehensionRootNode(
        TruffleLanguage<?> language,
        FrameDescriptor frameDescriptor,
        Pattern pattern,
        Expr guard,
        Expr result
    ) {
        super(language, frameDescriptor);
        this.pattern = pattern;
        this.guard = guard;
        this.result = result;
        this.toBoolean = LKQLToBooleanNodeGen.create();
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.root_nodes.BaseRootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object execute(VirtualFrame frame) {
        this.initFrame(frame);

        // by convention only 2 args [closure, arg]
        // see QueryComprehension
        var arg = frame.getArguments()[1];

        // pattern does not match -> early exit
        if (!this.pattern.executeValue(frame, arg)) return null;

        // guard present and evaluates to false -> early exit
        if (this.guard != null && !toBoolean.execute(guard.executeGeneric(frame))) return null;

        // result present -> evaluate result
        // else default to arg
        return this.result != null ? this.result.executeGeneric(frame) : arg;
    }

    @Override
    public String toString() {
        return (
            "<querycomp>:" +
            this.result.getLocation().fileName() +
            ":" +
            this.result.getLocation().startLine()
        );
    }
}
