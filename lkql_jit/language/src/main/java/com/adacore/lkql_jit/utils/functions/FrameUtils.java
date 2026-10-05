//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.adacore.lkql_jit.runtime.Cell;
import com.oracle.truffle.api.frame.VirtualFrame;

/** Util functions to manipulate Truffle frames with boundaries. */
public final class FrameUtils {

    /** Get the local value stored in the frame at the given slot. */
    public static Object readLocal(final VirtualFrame frame, final int slot) {
        return ((Cell) frame.getObject(slot)).getRef();
    }

    /** Write the slot in the given frame with the provided value. */
    public static void writeLocal(final VirtualFrame frame, final int slot, final Object value) {
        ((Cell) frame.getObject(slot)).setRef(value);
    }
}
