//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.langkit_translator.passes.Hierarchy;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.AdaNodeProxy;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
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
        // Build pass call-chain
        final Deque<LKQLFunction> callChain = new ArrayDeque<>();
        callChain.push((LKQLFunction) FrameUtils.readLocal(frame, slot));
        while (((PassExpr) callChain.peek().rootNode.getBody()).getPreviousSlot().isPresent()) {
            callChain.push(
                (LKQLFunction) FrameUtils.readLocal(
                    frame,
                    ((PassExpr) callChain.peek().rootNode.getBody()).getPreviousSlot().get()
                )
            );
        }

        // Setup all units roots
        final var roots = LKQLLanguage.getContext(this).getAllUnitsRoots();
        final var units = new Object[roots.length];
        for (int i = 0; i < roots.length; i++) {
            if (LKQLLanguage.getContext(this).isVerbose()) {
                System.out.println(i + ")\n" + roots[i].dumpTree());
            }
            units[i] = AdaNodeProxy.convertAST(roots[i]);
        }

        LKQLLanguage.getContext(this).setTypingContext(Hierarchy.initial());

        do {
            final var pass = callChain.pop();

            if (LKQLLanguage.getContext(this).isVerbose()) {
                System.out.println("running pass:" + pass);
            }

            for (int i = 0; i < units.length; i++) {
                try {
                    units[i] = InteropLibrary.getUncached().execute(pass, frame, units[i]);
                } catch (
                    UnsupportedTypeException | ArityException | UnsupportedMessageException e
                ) {
                    e.printStackTrace();
                }

                if (LKQLLanguage.getContext(this).isVerbose()) {
                    System.out.println(i + ")\n" + units[i]);
                }
            }
        } while (!callChain.isEmpty());

        return units;
    }

    public int getSlot() {
        return slot;
    }

    @Override
    public String toString(int indentLevel) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toString'");
    }
}
