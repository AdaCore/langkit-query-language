//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils;

import java.util.List;
import java.util.Map;

/**
 * This record contains the result of an LKQL config file parsing.
 *
 * @author Hugo GUERRIER
 */
public record LKQLConfigFileResult(
        List<String> allRules,
        List<String> adaRules,
        List<String> sparkRules,
        Map<String, String> aliases,
        Map<String, Map<String, Object>> args) {}
