//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.interfaces;

import com.oracle.truffle.api.interop.TruffleObject;

/** This interface defines the LKQL values basic interface. */
public interface LKQLValue extends TruffleObject {
    /** Get a documentation string for the LKQL value. */
    default String lkqlDocumentation() {
        return "";
    }

    /** Get a string representing the LKQL colling profile. */
    default String lkqlProfile() {
        return "TODO : Implement the profile information";
    }
}
