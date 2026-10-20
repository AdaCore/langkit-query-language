//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interfaces;

/** Interface for iterable LKQL values. */
public interface Iterable {
    /** Get an object to iterate on values stored in this iterable value. */
    Iterator iterator();
}
