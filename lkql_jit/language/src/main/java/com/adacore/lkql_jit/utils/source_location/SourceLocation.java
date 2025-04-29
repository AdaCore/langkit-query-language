//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.source_location;

import java.io.File;

/**
 * Abstract interface for a SourceLocation, used to bridge the world between the queried language,
 * that has no truffle sources, and the LKQL language.
 */
public interface SourceLocation {
    /** Return the 1-indexed start line for this location */
    public int startLine();

    /** Return the 1-indexed end line for this location */
    public int endLine();

    /** Return the 1-indexed start column for this location */
    public int startColumn();

    /** Return the 1-indexed end column for this location */
    public int endColumn();

    /**
     * Return the lines of the source that this location spans, including the text outside of the
     * strict span of this location, column-wise.
     */
    public String[] getLines();

    /** Return the name of the file for this source location. */
    public String fileName();

    /** Return the directory of the file this source location is located in. */
    public File getDir();

    /** Return the location as a <line>:<column> string */
    public default String display() {
        return display(false);
    }

    public default String display(boolean gnuFormat) {
        return (
            fileName() +
            ":" +
            startLine() +
            ":" +
            (gnuFormat ? String.format("%02d", startColumn()) : startColumn())
        );
    }
}
