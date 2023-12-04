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

package com.adacore.lkql_jit.built_ins.values.bases;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.oracle.truffle.api.interop.InteropLibrary;

/** This class is the base for all array like LKQL values. */
public abstract class ArrayLKQLValue extends BasicLKQLValue {
    /**
     * Internal method to perform equality checking on two LKQL array like values.
     *
     * @param lefts Interop library to access 'left' messages.
     * @param rights Interop library to access 'right' messages.
     * @param leftElems Interop library to access 'left' elements messages.
     * @param rightElems Interop library to access 'right' elements messages.
     */
    protected static boolean arrayValueEquals(
            ArrayLKQLValue left,
            ArrayLKQLValue right,
            InteropLibrary lefts,
            InteropLibrary rights,
            InteropLibrary leftElems,
            InteropLibrary rightElems) {
        try {
            // Get the left tuple size and compare it with the right tuple
            long size = lefts.getArraySize(left);
            if (size != rights.getArraySize(right)) return false;

            // Then compare each element of the tuples
            for (long i = 0; i < size; i++) {
                Object leftElem = lefts.readArrayElement(left, i);
                Object rightElem = rights.readArrayElement(right, i);
                if (leftElems.hasIdentity(leftElem)) {
                    if (!leftElems.isIdentical(leftElem, rightElem, rightElems)) return false;
                } else {
                    if (!ObjectUtils.equals(leftElem, rightElem)) return false;
                }
            }

            // If we get here, tuples are equal
            return true;
        } catch (Exception e) {
            throw LKQLRuntimeException.shouldNotHappen("Tuples comparison");
        }
    }

    /**
     * Internal method to compute the hash code of a given LKQL array like value.
     *
     * @param receivers Interop library to access the receiver messages.
     * @param elems Interop library to access the receiver elements messages.
     */
    protected static int arrayValueHashCode(
            ArrayLKQLValue receiver, InteropLibrary receivers, InteropLibrary elems) {
        try {
            final long size = receivers.getArraySize(receiver);
            int result = 1;
            for (long i = 0; i < size; i++) {
                Object elem = receivers.readArrayElement(receiver, i);
                result =
                        31 * result
                                + (elems.hasIdentity(elem)
                                        ? elems.identityHashCode(elem)
                                        : ObjectUtils.hashCode(elem));
            }
            return result;
        } catch (Exception e) {
            throw LKQLRuntimeException.shouldNotHappen(
                    "Error in array like value hash code computing");
        }
    }
}
