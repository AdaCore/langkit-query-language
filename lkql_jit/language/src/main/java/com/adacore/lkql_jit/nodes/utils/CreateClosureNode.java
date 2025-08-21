//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.utils;

import com.adacore.lkql_jit.runtime.Cell;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;

/**
 * Node to optimize the creation of a closure.
 */
public class CreateClosureNode extends Node {

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    int[] destinationSlots;

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    ClosureDescriptor.DestKind[] destinationKinds;

    public CreateClosureNode(ClosureDescriptor closureDescriptor) {
        this.destinationKinds = closureDescriptor.destinationKinds;
        this.destinationSlots = closureDescriptor.destinationSlots;
    }

    @ExplodeLoop(kind = ExplodeLoop.LoopExplosionKind.FULL_EXPLODE)
    public Closure execute(VirtualFrame frame) {
        CompilerAsserts.compilationConstant(destinationSlots.length);

        // Create the content of the closure
        final Cell[] content = new Cell[destinationSlots.length];

        for (int i = 0; i < destinationSlots.length; i++) {
            switch (destinationKinds[i]) {
                case LOCAL -> content[i] = (Cell) frame.getObject(destinationSlots[i]);
                case PARAM -> content[i] = new Cell(frame.getArguments()[destinationSlots[i]]);
                case CLOSURE -> content[i] =
                    ((Cell[]) frame.getArguments()[0])[destinationSlots[i]];
            }
        }

        // Return the new closure
        return new Closure(content);
    }
}
