//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exception;

import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import java.io.Serial;

/**
 * This class represents an exception from a Langkit call.
 *
 * @author Hugo GUERRIER
 */
public final class LangkitException extends AbstractTruffleException {

    // ----- Attributes -----

    @Serial private static final long serialVersionUID = 1755847711876252095L;

    /** Kind of the Langkit exception. */
    private final String kind;

    /** Message of the exception. */
    private final String msg;

    /** Location of the node which rose this error. */
    private final SourceLocation location;

    // ----- Constructors -----

    /**
     * Create a new Langkit exception.
     *
     * @param kind The kind of the exception.
     * @param msg The message of the exception.
     * @param location The location of the exception.
     */
    public LangkitException(String kind, String msg, SourceLocation location) {
        this.kind = kind;
        this.msg = msg;
        this.location = location;
    }

    // ----- Getters -----

    public String getKind() {
        return this.kind;
    }

    public String getMsg() {
        return this.msg;
    }

    public SourceLocation getLoc() {
        return location;
    }
}
