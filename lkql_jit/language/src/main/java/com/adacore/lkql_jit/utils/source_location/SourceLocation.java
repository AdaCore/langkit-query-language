/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.utils.source_location;

import com.adacore.liblkqllang.Liblkqllang;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

import java.io.File;


/**
 * This class represents a source location
 *
 * @author Hugo GUERRIER
 */
public final class SourceLocation {

    // ----- Attributes -----

    /**
     * The source
     */
    private final Source source;

    /**
     * The starting line of the source location
     */
    private final int startLine;

    /**
     * The starting column of the source location
     */
    private final short startColumn;

    /**
     * The ending line of the source location
     */
    private final int endLine;

    /**
     * The ending column of the source location
     */
    private final short endColumn;

    // ----- Constructors -----

    /**
     * Create a source location from a langkit source location range
     *
     * @param source        The source
     * @param locationRange The source location range from langkit
     */
    public SourceLocation(
        Source source,
        Liblkqllang.SourceLocationRange locationRange
    ) {
        this(
            source,
            locationRange.start.line,
            locationRange.start.column,
            locationRange.end.line,
            locationRange.end.column
        );
    }

    /**
     * Create a new source location from the lines and columns
     *
     * @param source      The source
     * @param startLine   The starting line
     * @param startColumn The starting column
     * @param endLine     The ending line
     * @param endColumn   The ending column
     */
    public SourceLocation(
        Source source,
        int startLine,
        short startColumn,
        int endLine,
        short endColumn
    ) {
        this.source = source;
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.endLine = endLine;
        this.endColumn = endColumn;
    }

    // ----- Getters -----

    public Source getSource() {
        return source;
    }

    public int getStartLine() {
        return startLine;
    }

    public short getStartColumn() {
        return startColumn;
    }

    public int getEndLine() {
        return endLine;
    }

    public short getEndColumn() {
        return endColumn;
    }

    // ----- Source accessing methods -----

    /**
     * Get the source file name
     *
     * @return The source file name
     */
    @CompilerDirectives.TruffleBoundary
    public String getFileName() {
        return this.source.getName();
    }

    /**
     * Get the module name (the file name without extension)
     *
     * @return The module name
     */
    @CompilerDirectives.TruffleBoundary
    public String getModuleName() {
        return this.source.getName().split("\\.")[0];
    }

    /**
     * Get the current working directory
     *
     * @return The current directory or null if the source is command line
     */
    @CompilerDirectives.TruffleBoundary
    public File getCurrentDir() {
        if (this.source.getPath() != null) {
            return new File(this.source.getPath()).getParentFile();
        } else {
            return null;
        }
    }

    /**
     * Get all the lines from the source
     *
     * @return All the text lines from the source
     */
    @CompilerDirectives.TruffleBoundary
    public String[] getLines() {
        return this.source.getCharacters().toString().split(System.lineSeparator());
    }

    /**
     * Get the lines from the source between the start and end index
     *
     * @param start The start line (included)
     * @param end   The end line (excluded)
     * @return The lines from the source
     */
    public String[] getLines(int start, int end) {
        // Verify the argument validity
        if (end <= start) {
            return new String[0];
        }

        // Prepare the result
        String[] res = new String[end - start];
        for (int i = start; i < end; i++) {
            res[i - start] = this.source.getCharacters(i + 1).toString();
        }

        // Return the result
        return res;
    }

    /**
     * Create a source section for the compiler debugging
     *
     * @return The source section of the node
     */
    public SourceSection createSection() {
        return this.source.createSection(
            this.startLine,
            this.startColumn,
            this.endLine,
            this.endColumn
        );
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        return this.getFileName() + ":" + this.startLine + ":" + this.startColumn;
    }

}
