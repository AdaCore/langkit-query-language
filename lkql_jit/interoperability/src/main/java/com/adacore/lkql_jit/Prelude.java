//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import java.util.Scanner;

/** Class holding the LKQL prelude and methods to access it. */
public final class Prelude {

    /** Get the LKQL prelude as a string. */
    public static String getPreludeText() {
        // Stupid Scanner Trick
        try (
            final var s = new Scanner(
                Prelude.class.getResourceAsStream("/prelude.lkql")
            ).useDelimiter("\\A")
        ) {
            return s.next();
        }
    }
}
