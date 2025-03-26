//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime;

import java.io.File;
import java.util.Stack;
import java.util.stream.Collectors;

public class ImportationStack {

    // ----- Attributes -----

    public final Stack<File> importations = new Stack<>();

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.importations.stream().map(File::getName).collect(Collectors.joining(" -> "));
    }
}
