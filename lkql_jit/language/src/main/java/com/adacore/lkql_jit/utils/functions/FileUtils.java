//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import java.io.File;

/**
 * Util functions for the file java class manipulation in the JIT implementation.
 *
 * @author Hugo GUERRIER
 */
public final class FileUtils {

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

    /** Get the full path of a source if possible, otherwise get its name. */
    public static String sourcePathOrName(Source source) {
        var sourcePath = source.getPath();
        return sourcePath != null ? sourcePath : source.getName();
    }
}
