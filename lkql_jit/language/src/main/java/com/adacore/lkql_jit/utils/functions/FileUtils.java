//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.LKQLLanguage;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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

    /** Return all directories in which to look for LKQL scripts. */
    public static List<File> lkqlSearchDirs() {
        final String lkqlPath = System.getenv().getOrDefault(Constants.LKQL_PATH, "");
        final List<File> searchDirs = new ArrayList<>();

        searchDirs.addAll(
            Arrays.stream(StringUtils.splitPaths(lkqlPath))
                .filter(s -> !s.isBlank())
                .map(File::new)
                .toList()
        );
        searchDirs.addAll(
            LKQLLanguage.getContext(null).getAdditionalLkqlPaths().stream().map(File::new).toList()
        );

        return searchDirs;
    }
}
