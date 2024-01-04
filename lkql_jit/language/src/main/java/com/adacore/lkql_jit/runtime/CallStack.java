//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime;

import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.oracle.truffle.api.CompilerDirectives;
import java.util.LinkedList;

/** This class represents a call stack for the LKQL language. */
public final class CallStack implements Cloneable {

    // ----- Attributes -----

    /** This is where all calls are stored. */
    public final LinkedList<Element> calls = new LinkedList<>();

    // ----- Instance methods -----

    public boolean isEmpty() {
        return this.calls.isEmpty();
    }

    @CompilerDirectives.TruffleBoundary
    public void pushCall(LKQLFunction function, FunCall call) {
        this.calls.addFirst(new Element(function, call));
    }

    @CompilerDirectives.TruffleBoundary
    public void popCall() {
        this.calls.removeFirst();
    }

    // ----- Override methods -----

    @Override
    public CallStack clone() {
        var res = new CallStack();
        res.calls.addAll(this.calls);
        return res;
    }

    // ----- Inner classes -----

    /**
     * An element of a call stack is an association between an LKQL functional value and a call
     * node.
     */
    public record Element(LKQLFunction function, FunCall call) {
        public String display() {
            return this.function.getName()
                    + " (called at "
                    + this.call.getLocation().display()
                    + ")";
        }
    }
}
