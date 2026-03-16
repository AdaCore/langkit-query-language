//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ClosureDescriptor;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.interop.LKQLCollection;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.adacore.lkql_jit.values.streams.LKQLConsStream;
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
    protected LKQLConsStream onList(VirtualFrame frame, LKQLCollection list) {
        return LKQLConsStream.concatStream(
            list,
            this.tailLazyValue.getCallTarget(),
            this.createTailClosure.execute(frame)
        );
    }

    @Specialization
    protected LKQLConsStream onStream(VirtualFrame frame, LKQLStream stream) {
        return LKQLConsStream.concatStream(
            stream,
            this.tailLazyValue.getCallTarget(),
            this.createTailClosure.execute(frame)
        );
    }

    @Fallback
    protected void invalidType(Object o) {
        throw LKQLRuntimeError.wrongType(
            LKQLTypesHelper.LKQL_LIST,
            LKQLTypesHelper.fromJava(o),
            this.getHead()
        );
    }
}
