/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.utils;

import java.io.IOException;
import java.io.Writer;

public class TextWriter implements AutoCloseable {

    private int indent;

    private final Writer writer;

    public void withIndent(Runnable r) {
        this.indent += 4;
        r.run();
        this.indent -= 4;
    }

    public TextWriter(Writer writer) {
        this.writer = writer;
    }

    public void write(String str) {
        try {
            var lines = str.split("\\n", -1);
            for (int i = 0; i < lines.length; i++) {
                writeRaw(lines[i]);
                if (i != lines.length - 1) {
                    this.writer.write("\n");
                }
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void writeRaw(String str) throws IOException {
        for (int i = 0; i < indent; i++) {
            this.writer.write(" ");
        }
        this.writer.write(str);
    }

    @Override
    public void close() throws IOException {
        this.writer.close();
    }
}
