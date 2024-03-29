//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exception.utils;

import com.oracle.truffle.api.CompilerDirectives;
import java.io.Serial;

/**
 * This exception represents an invalid index access in LKQL.
 *
 * @author Hugo GUERRIER
 */
public final class InvalidIndexException extends RuntimeException {
    @Serial private static final long serialVersionUID = 6248743740479992497L;

    @CompilerDirectives.TruffleBoundary
    public InvalidIndexException() {}
}
