//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import java.util.HashMap;

/**
 * DynamicAdaNode
 * dynamically typed runtime representation of an AST node
 * used in all nanopasses but the first one (see {@link AdaNodeProxy})
 */
public class DynamicAdaNode extends BasicLKQLValue {

    public final String kind;
    public final HashMap<String, DynamicAdaNode> children;
    public final HashMap<String, LKQLValue> fields;

    public DynamicAdaNode(
        String kind,
        HashMap<String, DynamicAdaNode> children,
        HashMap<String, LKQLValue> fields
    ) {
        this.kind = kind;
        this.children = children;
        this.fields = fields;
    }

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
                fields.put(key, (LKQLValue) arg);
            }
        }
    }

    public Object getField(String name) {
        var res = fields.get(name);
        return res != null ? res : children.get(name);
    }
}
