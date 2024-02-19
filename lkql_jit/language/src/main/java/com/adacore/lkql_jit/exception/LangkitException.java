/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

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

    @Serial
    private static final long serialVersionUID = 1755847711876252095L;

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
