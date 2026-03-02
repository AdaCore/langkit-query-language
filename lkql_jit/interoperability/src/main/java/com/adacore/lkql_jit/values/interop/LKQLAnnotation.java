//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.interop;

import java.util.List;
import java.util.Map;

/** This record represents an annotation associated to an LKQL value. */
public record LKQLAnnotation(
    String name,
    List<Object> positionalArguments,
    Map<String, Object> namedArguments
) {}
