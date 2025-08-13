//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.langkit_translator.passes.Hierarchy;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.AdaNodeProxy;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Special annotation to specify the entry point for the nanopass framework.
 * The pass designated by the associate identifier and all its transitive
 * dependencies are going to be executed.
 */
public class RunPass extends LKQLNode {

    final int slot;

    public RunPass(SourceSection location, int slot) {
        super(location);
        this.slot = slot;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        final var ctx = LKQLLanguage.getContext(this);

        // Build pass call-chain
        final Deque<LKQLFunction> callChain = new ArrayDeque<>();
        callChain.push((LKQLFunction) FrameUtils.readLocal(frame, slot));
        while (((PassExpr) callChain.peek().rootNode.getBody()).getPreviousSlot().isPresent()) {
            callChain.push(
                (LKQLFunction) FrameUtils.readLocal(
                    frame,
                    // orElse is necessary to garantee no exception for native image
                    ((PassExpr) callChain.peek().rootNode.getBody()).getPreviousSlot().orElse(0)
                )
            );
        }

        // Setup all units roots
        final var roots = ctx.getAllUnitsRoots();
        final var units = new Object[roots.length];
        for (int i = 0; i < roots.length; i++) {
            if (ctx.isVerbose()) {
                debugAST(i, roots[i].dumpTree());
            }
            units[i] = AdaNodeProxy.convertAST(roots[i]);
        }

        ctx.setTypingContext(Hierarchy.initial());

        do {
            final var pass = callChain.poll();

            if (ctx.isVerbose()) {
                ctx.println(ObjectUtils.toString(pass));
            }

            for (int i = 0; i < units.length; i++) {
                try {
                    units[i] = InteropLibrary.getUncached().execute(pass, frame, units[i]);
                } catch (
                    UnsupportedTypeException | ArityException | UnsupportedMessageException e
                ) {
                    LKQLRuntimeException.fromJavaException(e, this);
                }

                if (ctx.isVerbose()) {
                    debugAST(i, units[i]);
                }
            }
        } while (!callChain.isEmpty());

        return units;
    }

    public int getSlot() {
        return slot;
    }

    @TruffleBoundary
    private void debugAST(int num, Object ast) {
        System.out.println(num + ")\n" + ast);
    }

    @Override
    public String toString(int indentLevel) {
        return nodeRepresentation(indentLevel, new String[] { "slot" }, new Object[] { slot });
    }
}
