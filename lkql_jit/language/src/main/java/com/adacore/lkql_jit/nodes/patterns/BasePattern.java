//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents the base for all patterns in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BasePattern extends LKQLNode {

    /**
     * Create a new base pattern node.
     *
     * @param location The location of the node in the source.
     */
    protected BasePattern(SourceSection location) {
        super(location);
    }

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public final Object executeGeneric(VirtualFrame frame) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    /**
     * Execute the pattern and get if the node fulfills it.
     *
     * @param frame The frame to execute the pattern in.
     * @param value The node to verify.
     * @return True of the node verify the pattern, false else.
     */
    public abstract boolean executeValue(VirtualFrame frame, Object value);
}
