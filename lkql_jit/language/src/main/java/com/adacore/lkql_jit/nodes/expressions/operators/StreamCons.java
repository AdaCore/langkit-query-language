//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ClosureDescriptor;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.values.lists.LKQLConsStream;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This class represents the "cons" constructor for streams, creating a stream from its head and
 * its lazy tail.
 * This class doesn't extend BinOp because the right children shouldn't be evaluated eagerly.
 */
public abstract class StreamCons extends BaseStreamOp {

    // ----- Constructor -----

    /** Create a new stream constructor node. */
    protected StreamCons(
        SourceSection location,
        Expr tail,
        FrameDescriptor tailFrameDescriptor,
        ClosureDescriptor tailClosureDescriptor
    ) {
        super(location, tail, tailFrameDescriptor, tailClosureDescriptor);
    }

    // ----- Execution methods -----

    @Specialization
    protected LKQLConsStream onAny(VirtualFrame frame, Object head) {
        return new LKQLConsStream(
            head,
            this.tailLazyValue.getCallTarget(),
            createTailClosure.execute(frame)
        );
    }
}
