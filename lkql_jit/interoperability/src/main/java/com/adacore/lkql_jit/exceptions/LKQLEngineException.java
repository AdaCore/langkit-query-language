//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exceptions;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import java.io.Serial;

/**
 * This exception class represents an error that occurred during the execution of an LKQL source.
 * This class is only about bugs in the LKQL engine, LKQL runtime errors are represented by
 * {@link LKQLRuntimeError}, LKQL static errors are reported using the {@link LKQLStaticErrors}
 * class.
 */
public class LKQLEngineException extends AbstractTruffleException {

    // ----- Attributes -----

    @Serial
    private static final long serialVersionUID = 2895637825892140976L;

    // ----- Constructors -----

    private LKQLEngineException(Throwable cause, Node location) {
        super(cause.getMessage(), cause, -1, location);
    }

    private LKQLEngineException(String message, Node location) {
        super(message, location);
    }

    private LKQLEngineException(String message) {
        this(message, null);
    }

    // ----- Creation methods -----

    public static LKQLEngineException create(String message) {
        return new LKQLEngineException(message);
    }

    public static LKQLEngineException create(Throwable cause) {
        return new LKQLEngineException(cause, null);
    }

    public static LKQLEngineException create(Throwable cause, Node location) {
        return new LKQLEngineException(cause, location);
    }

    public static LKQLEngineException shouldNotReachHere() {
        return new LKQLEngineException("Execution should not reach here");
    }
}
