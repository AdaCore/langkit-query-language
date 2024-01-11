//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

/** This enum represents the mode for the auto fixes application. */
public enum AutoFixMode {
    /** Display the patched analysis unit to stdout. */
    DISPLAY,

    /** Create a new file alongside the original one, containing the patched analysis unit. */
    NEW_FILE,

    /** Replace the content of the original file with the patched analysis unit. */
    PATCH_FILE
}
