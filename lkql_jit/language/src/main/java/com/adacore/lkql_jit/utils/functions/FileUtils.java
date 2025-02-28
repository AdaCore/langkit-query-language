//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import java.io.File;

/**
 * Util functions for the file java class manipulation in the JIT implementation.
 *
 * @author Hugo GUERRIER
 */
public final class FileUtils {

    /** Create a new file object from its name. */
    @CompilerDirectives.TruffleBoundary
    public static File create(String fileName) {
        return new File(fileName);
    }

    /**
     * Get a file base name from a file path.
     *
     * @param filePath The file path.
     * @return The file base name.
     */
    @CompilerDirectives.TruffleBoundary
    public static String baseName(String filePath) {
        return new File(filePath).getName();
    }
}
