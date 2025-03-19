//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.runtime.values.lists.BaseLKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

@GenerateInline(false)
public abstract class LKQLToBoolean extends Node {

    public abstract boolean execute(Object value);

    @Override
    public SourceSection getSourceSection() {
        // Since this node is inline it's not practical to give it a location
        // explicitly, so we just return the parent's location.
        return this.getParent().getSourceSection();
    }

    @Specialization
    protected boolean onBoolean(boolean val) {
        return val;
    }

    @Specialization
    protected boolean onBaseLKQLList(BaseLKQLList list) {
        return list.asBoolean();
    }

    @Specialization
    protected boolean onLKQLNull(LKQLNull lkqlNull) {
        return false;
    }

    @Specialization
    protected boolean onLKQLUnit(LKQLUnit unit) {
        return false;
    }

    @Specialization
    protected boolean onNode(LangkitSupport.NodeInterface node) {
        return checkNode(node);
    }

    @CompilerDirectives.TruffleBoundary
    protected boolean checkNode(LangkitSupport.NodeInterface node) {
        return !node.isNone();
    }

    @Fallback
    protected boolean onOther(Object other) {
        throw LKQLRuntimeException.wrongType(
            LKQLTypesHelper.LKQL_BOOLEAN,
            LKQLTypesHelper.fromJava(other),
            this
        );
    }
}
