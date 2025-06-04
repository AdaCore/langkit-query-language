//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.runtime.values.DynamicAdaNode;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.util.HashMap;

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
            if (arg instanceof NamedArg) continue;
            throw LKQLRuntimeException.fromMessage("constructors in passes only accept named args");
        }
    }

    public int arity() {
        return this.args.getArgs().length;
    }

    // ----- Execution methods -----

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        final var res = new DynamicAdaNode(nodeKind, new HashMap<>(), new HashMap<>());
        final var argResults = this.args.executeArgList(frame);
        for (int i = 0; i < argResults.length; i++) {
            final var name = ((NamedArg) this.args.getArgs()[i]).getArgName().getName();
            if (argResults[i] instanceof DynamicAdaNode child) {
                res.children.put(name, child);
            } else {
                res.fields.put(name, (LKQLValue) argResults[i]);
            }
        }

        return res;
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
