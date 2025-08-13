//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import java.util.HashMap;

/**
 * A dynamically typed runtime representation of an AST node
 * used in all nanopasses but the first one (see {@link AdaNodeProxy})
 */
public class DynamicAdaNode extends BasicLKQLValue {

    public final String kind;
    public final HashMap<String, DynamicAdaNode> children;
    public final HashMap<String, Object> fields;
    public final boolean isList;

    /**
     * This constructor is the one used during lowering
     * and should be the default one
     */
    @TruffleBoundary
    public DynamicAdaNode(
        String kind,
        HashMap<String, DynamicAdaNode> children,
        HashMap<String, Object> fields,
        boolean isList
    ) {
        this.kind = kind;
        this.children = children;
        this.fields = fields;
        this.isList = isList;
    }

    /**
     * This constructor is used only in the context of a nanopass
     * by a constructor call
     */
    @TruffleBoundary
    public DynamicAdaNode(String kind, Object[] args, String[] argnames) {
        this.kind = kind;
        this.children = new HashMap<>();
        this.fields = new HashMap<>();
        for (int i = 0; i < args.length; i++) {
            var arg = args[i];
            var key = argnames[i];
            if (arg instanceof DynamicAdaNode child) {
                children.put(key, child);
            } else {
                fields.put(key, arg);
            }
        }
        this.isList = false;
    }

    @TruffleBoundary
    public Object getField(String name) {
        var res = fields.get(name);
        return res != null ? res : children.get(name);
    }

    public String toString() {
        final var sb = new StringBuilder();
        dump(sb, "");
        return sb.toString();
    }

    public void dump(StringBuilder sb, String prefix) {
        sb.append(prefix);
        sb.append(kind);
        sb.append('\n');

        children
            .entrySet()
            .stream()
            .forEach(e -> {
                sb.append(prefix);
                sb.append("|");
                sb.append(e.getKey());
                sb.append(':');
                sb.append('\n');
                e.getValue().dump(sb, prefix + "|  ");
            });

        fields
            .entrySet()
            .stream()
            .forEach(e -> {
                sb.append(prefix);
                sb.append("|");
                sb.append(e);
                sb.append('\n');
            });
    }
}
