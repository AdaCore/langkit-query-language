//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exception;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.source.SourceSection;
import java.io.Serial;

/**
 * This class represents an exception from a Langkit call.
 *
 * @author Hugo GUERRIER
 */
public final class LangkitException extends AbstractTruffleException {

    // ----- Attributes -----

    @Serial
    private static final long serialVersionUID = 1755847711876252095L;

    /** Message of the exception. */
    private final String msg;

    /** Location of the node which rose this error. */
    private final SourceSection location;

    // ----- Constructors -----

    /**
     * Create a new Langkit exception.
     *
     * @param msg The message of the exception.
     * @param location The location of the exception.
     */
    public LangkitException(String msg, SourceSection location) {
        this.msg = msg;
        this.location = location;
    }

    // ----- Getters -----

    public String getMsg() {
        return this.msg;
    }

    public SourceSection getLoc() {
        return location;
    }
}
