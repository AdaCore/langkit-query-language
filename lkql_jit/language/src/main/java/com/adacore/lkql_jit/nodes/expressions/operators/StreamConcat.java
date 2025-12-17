//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ClosureDescriptor;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.lists.BaseLKQLList;
import com.adacore.lkql_jit.runtime.values.lists.LKQLStream;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/** This class represents a node to concatenate any list value to a stream. */
public abstract class StreamConcat extends BaseStreamOp {

    /** Create a new stream concatenation node. */
    protected StreamConcat(
        SourceSection location,
        Expr tail,
        FrameDescriptor tailFrameDescriptor,
        ClosureDescriptor tailClosureDescriptor
    ) {
        super(location, tail, tailFrameDescriptor, tailClosureDescriptor);
    }

    // ----- Execution methods -----

    @Specialization
    protected LKQLStream onList(VirtualFrame frame, BaseLKQLList list) {
        return new LKQLStream.LKQLComposedStream(
            list,
            this.tailLazyValue.getCallTarget(),
            this.createTailClosure.execute(frame)
        );
    }

    @Fallback
    protected void invalidType(Object o) {
        throw LKQLRuntimeException.wrongType(
            LKQLTypesHelper.LKQL_LIST,
            LKQLTypesHelper.fromJava(o),
            this.getHead()
        );
    }
}
