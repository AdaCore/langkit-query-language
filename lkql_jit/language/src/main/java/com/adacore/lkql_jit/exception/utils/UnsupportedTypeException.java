//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exception.utils;

import java.io.Serial;

/**
 * This exception represents an error in the importation of a foreign value in the LKQL system.
 *
 * @author Hugo GUERRIER
 */
public final class UnsupportedTypeException extends Exception {

    @Serial
    private static final long serialVersionUID = 7197470955602340792L;

    private final Class<?> type;

    public UnsupportedTypeException(Class<?> type) {
        this.type = type;
    }

    public Class<?> getType() {
        return type;
    }
}
