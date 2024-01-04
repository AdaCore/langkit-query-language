//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.bases;

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
