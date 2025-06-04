//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.libadalang.Libadalang;
import com.adacore.libadalang.Libadalang.AdaNode;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
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

    public static AdaNodeProxy convertAST(AdaNode root) {
        return new AdaNodeProxy(root);
    }

    private AdaNodeProxy(AdaNode base) {
        super(base.getClass().getName(), new HashMap<>(), new HashMap<>());
        this.base = base; // keep a reference
        final String[] declaredFields = Libadalang.NODE_DESCRIPTION_MAP.get(kind).fields;
        for (String declaredField : declaredFields) {
            if (declaredField.startsWith("f")) {
                final AdaNode child = (AdaNode) callMethod(declaredField, new Object[0]);
                children.put(declaredField, new AdaNodeProxy(child));
            }
        }
    }

    public Object callMethod(String name, Object[] args) {
        Class<?>[] argtypes = new Class[args.length];
        for (int i = 0; i < args.length; i++) {
            argtypes[i] = args[i].getClass();
        }

        Method method = null;
        try {
            method = base.getClass().getMethod(name, argtypes);
        } catch (NoSuchMethodException | SecurityException e) {
            throw LKQLRuntimeException.fromJavaException(e, null);
        }

        if (method == null) {
            return null;
        }

        Object result = null;
        try {
            result = method.invoke(base, args);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw LKQLRuntimeException.fromJavaException(e, null);
        }
        return result;
    }
}
