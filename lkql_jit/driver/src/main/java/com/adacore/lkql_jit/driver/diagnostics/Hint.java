//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.source_support.SourceSection;

/**
 * Represents a hint for a diagnostic. A hint is a located additional information you want to attach
 * to a diagnostic.
 */
public record Hint(String message, SourceSection location) {}
