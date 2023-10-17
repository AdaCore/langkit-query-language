/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import java.util.Objects;

/**
 * This class represents the tuple values in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class TupleValue implements Indexable {

    // ----- Attributes -----

    /** The values contained in the tuple. */
    private final Object[] content;

    // ----- Constructors -----

    /**
     * Create a new tuple value.
     *
     * @param content The content of the tuple.
     */
    public TupleValue(Object[] content) {
        this.content = content;
    }

    // ----- Value methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.Indexable#get(int)
     */
    @Override
    public Object get(int index) throws InvalidIndexException {
        if (index < 0 || index >= this.content.length) {
            throw new InvalidIndexException();
        }
        return this.content[index];
    }

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.Indexable#getContent()
     */
    @Override
    public Object[] getContent() {
        return this.content;
    }

    /**
     * @see
     *     com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean internalEquals(LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof TupleValue other)) return false;
        if (other.content.length != this.content.length) return false;
        for (int i = 0; i < this.content.length; i++) {
            Object mineObject = this.content[i];
            Object hisObject = other.content[i];
            if ((mineObject instanceof LKQLValue mine) && (hisObject instanceof LKQLValue his)) {
                if (!mine.internalEquals(his)) return false;
            } else {
                if (!Objects.equals(mineObject, hisObject)) return false;
            }
        }
        return true;
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        // Create the tuple string in a builder
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        for (int i = 0; i < this.content.length; i++) {
            if (this.content[i] == null) {
                builder.append("null");
            } else if (this.content[i] instanceof String s) {
                builder.append(StringUtils.toRepr(s));
            } else {
                builder.append(this.content[i].toString());
            }
            if (i < this.content.length - 1) builder.append(", ");
        }
        builder.append(')');

        // Return the tuple string representation
        return builder.toString();
    }
}
