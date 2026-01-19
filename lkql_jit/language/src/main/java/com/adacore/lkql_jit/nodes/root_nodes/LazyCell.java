//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

/** This class represents any value that may be evaluated in a lazy way. */
public final class LazyCell extends BaseRootNode {

    // ----- Children -----

    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr value;

    // ----- Constructors -----

    public LazyCell(TruffleLanguage<?> language, FrameDescriptor frameDescriptor, Expr value) {
        super(language, frameDescriptor);
        this.value = value;
    }

    // ----- Execution methods -----

    @Override
    public Object execute(VirtualFrame frame) {
        this.initFrame(frame);
        return this.value.executeGeneric(frame);
    }
}
