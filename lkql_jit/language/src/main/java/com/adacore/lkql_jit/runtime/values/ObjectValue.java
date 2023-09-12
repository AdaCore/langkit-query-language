/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;


/**
 * This class represents the object values in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class ObjectValue implements LKQLValue {

    // ----- Attributes -----

    /**
     * The content of the object.
     */
    private final Map<String, Object> content;

    // ----- Constructors -----

    /**
     * Create an object value.
     *
     * @param keys   The keys of the object.
     * @param values The values of the object.
     */
    @CompilerDirectives.TruffleBoundary
    public ObjectValue(
        String[] keys,
        Object[] values
    ) {
        this.content = new HashMap<>(keys.length);
        for (int i = 0; i < keys.length; i++) {
            this.content.put(keys[i], values[i]);
        }
    }

    // ----- Getters -----

    @CompilerDirectives.TruffleBoundary
    public Object get(String key) {
        return this.content.getOrDefault(key, null);
    }

    public boolean contains(String key) {
        return this.content.containsKey(key);
    }

    // ----- Value methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean internalEquals(LKQLValue o) {
        if (this == o) return true;
        if (!(o instanceof ObjectValue other)) return false;
        if (this.content.size() != other.content.size()) return false;
        for (String key : this.content.keySet()) {
            Object mineObject = this.content.getOrDefault(key, null);
            Object hisObject = other.content.getOrDefault(key, null);
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
        // Create the object string builder
        StringBuilder res = new StringBuilder();
        res.append("{");
        Iterator<String> keyIterator = this.content.keySet().stream()
            .sorted()
            .iterator();
        while (keyIterator.hasNext()) {
            String key = keyIterator.next();
            Object value = this.content.get(key);
            String valueString;
            if (value == null) {
                valueString = "null";
            } else if (value instanceof String s) {
                valueString = StringUtils.toRepr(s);
            } else {
                valueString = value.toString();
            }

            res.append('"').append(key).append('"');
            res.append(": ").append(valueString);
            if (keyIterator.hasNext()) {
                res.append(", ");
            }
        }
        res.append("}");

        // Return the result
        return res.toString();
    }

}
