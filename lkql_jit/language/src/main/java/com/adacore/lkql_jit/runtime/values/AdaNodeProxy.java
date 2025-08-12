//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.langkit_support.LangkitSupport.NodeInterface;
import com.adacore.libadalang.Libadalang;
import java.util.HashMap;

/**
 * This class is a proxy to a typed (native libadalang) ada node.
 * This allows the rewriting code to handle the native nodes and the dynamic
 * nodes transparently.
 * It's used only in the first nanopasss (see {@link DynamicAdaNode} for the
 * others), and it's then replaced by a DynamicAdaNode.
 */
public class AdaNodeProxy extends DynamicAdaNode {

    /** The native libadalang node this class proxies to */
    public final NodeInterface base;

    public static AdaNodeProxy convertAST(NodeInterface root) {
        return new AdaNodeProxy(root);
    }

    private AdaNodeProxy(NodeInterface root) {
        super(
            ((Libadalang.AdaNode) root).getClassName(),
            new HashMap<>(),
            new HashMap<>(),
            root.isListNode()
        );
        this.base = root; // keep a reference

        if (root.isNone()) {
            // early exit to avoid looking at fields
            return;
        }

        if (root.isListNode()) {
            for (int i = 0; i < root.getChildrenCount(); i++) {
                children.put("item_" + i, new AdaNodeProxy(root.getChild(i)));
            }
        } else {
            final var fieldNames = root.getFieldNames();
            for (int i = 0; i < fieldNames.length; i++) {
                children.put(fieldNames[i], new AdaNodeProxy(root.getChild(i)));
            }
        }
    }
}
