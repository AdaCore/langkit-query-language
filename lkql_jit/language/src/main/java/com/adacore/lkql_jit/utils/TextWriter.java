//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

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
