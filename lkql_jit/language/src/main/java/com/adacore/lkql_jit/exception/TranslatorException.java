//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exception;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import java.io.Serial;

/**
 * This exception represents an exception in the LKQL static analysis. This doesn't represent a
 * static error in the LKQL code.
 *
 * @author Hugo GUERRIER
 */
public final class TranslatorException extends AbstractTruffleException {

    @Serial
    private static final long serialVersionUID = -1480198635766066797L;

    /**
     * Create a new exception from its message.
     *
     * @param message The message of the exception.
     */
    public TranslatorException(final String message) {
        super(message);
    }
}
