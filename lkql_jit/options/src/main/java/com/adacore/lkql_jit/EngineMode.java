//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

/** Represents the mode the LKQL engine runs on. */
public enum EngineMode {
    /** LKQL engine is just going to run the provided LKQL script, without doing more. */
    INTERPRETER,

    /** LKQL engine will seek for defined rules and make them accessible. */
    CHECKER,

    /** LKQL engine will seek for rules and check that they all define an auto-fixing function. */
    FIXER
}
