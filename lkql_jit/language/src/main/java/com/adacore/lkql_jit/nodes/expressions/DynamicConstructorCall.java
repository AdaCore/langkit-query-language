//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.runtime.values.DynamicAdaNode;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a constructor call in the LKQL language.
 * It is used only in the context of a nanopass, contrary to
 * {@link ConstructorCall}. The type of node is decided in the lowering
 * pass, thanks to a flag set when entering a pass node.
 */
public final class DynamicConstructorCall extends Expr {

    // ----- Children -----

    @Child
    private ArgList args;

    // ----- Attributes -----

    /** Kind of the node to create. */
    public final String nodeKind;

    // ----- Constructors -----

    public DynamicConstructorCall(SourceSection location, String nodeKind, ArgList argList) {
        super(location);
        // Get the node kind and if this is a token node
        this.nodeKind = nodeKind;
        this.args = argList;
        for (var arg : argList.getArgs()) {
            if (!(arg instanceof NamedArg)) throw LKQLRuntimeException.fromMessage(
                "constructors in passes only accept named args",
                arg
            );
        }
    }

    public int arity() {
        return this.args.getArgs().length;
    }

    public String[] getArgNames() {
        final var argNames = new String[args.getArgs().length];
        for (int i = 0; i < argNames.length; i++) {
            argNames[i] = this.args.getArgs()[i].getArgStringName();
        }
        return argNames;
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        final Object[] argVals = new Object[arity()];
        for (int i = 0; i < arity(); i++) {
            argVals[i] = args.getArgs()[i].executeGeneric(frame);
        }
        return new DynamicAdaNode(nodeKind, argVals, this.getArgNames());
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(
            indentLevel,
            new String[] { "nodeKind" },
            new Object[] { this.nodeKind }
        );
    }
}
