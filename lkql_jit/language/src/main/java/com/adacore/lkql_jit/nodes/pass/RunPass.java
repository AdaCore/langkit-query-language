//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.LKQLLanguage;
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
        // get pass
        final var pass = (LKQLFunction) FrameUtils.readLocal(frame, slot);

        // setup results
        final var roots = LKQLLanguage.getContext(this).getAllUnitsRoots();
        final var res = new Object[roots.length];

        for (int i = 0; i < roots.length; i++) {
            final var root = roots[i];
            final AdaNodeProxy input = null; // TODO translate root of AST
            try {
                res[i] = InteropLibrary.getUncached().execute(pass, input);
            } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException e) {
                e.printStackTrace();
            }
        }

        return res;
    }

    @Override
    public String toString(int indentLevel) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toString'");
    }
}
