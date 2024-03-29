//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.adacore.lkql_jit.runtime.Cell;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * This class contains all util functions about the frame managing.
 *
 * @author Hugo GUERRIER
 */
public final class FrameUtils {

    /**
     * Get the local value stored in the frame at the given slot.
     *
     * @param frame The frame to read the local in.
     * @param slot The slot to read the local at.
     * @return The local value at the slot.
     */
    public static Object readLocal(final VirtualFrame frame, final int slot) {
        return ((Cell) frame.getObject(slot)).getRef();
    }

    /**
     * Write the local value at the given slot in the given frame.
     *
     * @param frame The frame to write the value in.
     * @param slot The slot to write the value at.
     * @param value The value to write.
     */
    public static void writeLocal(final VirtualFrame frame, final int slot, final Object value) {
        ((Cell) frame.getObject(slot)).setRef(value);
    }

    /**
     * Read the closure value stored in the frame at the given slot.
     *
     * @param frame The frame to read the closure value in.
     * @param slot The slot to read the closure at.
     * @return The closure value at the slot.
     */
    public static Object readClosure(final VirtualFrame frame, final int slot) {
        return (((Cell[]) frame.getArguments()[0])[slot]).getRef();
    }
}
