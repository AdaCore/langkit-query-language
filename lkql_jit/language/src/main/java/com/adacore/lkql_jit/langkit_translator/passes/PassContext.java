//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.pass.AddBlock;
import com.adacore.lkql_jit.nodes.pass.DelBlock;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

/**
 * This class is used during the typing pass of nanopasses
 * ({@link ResolutionPass}). It holds the typing environement during a pass,
 * and is updated at each step during pass verification to typecheck each pass.
 */
public class PassContext {

    /** maps a class name to its available members */
    public final HashMap<String, ClassDescriptor> env;

    private PassContext() {
        this.env = new HashMap<>();
    }

    public static PassContext initial() {
        PassContext ctx = new PassContext();
        Libadalang.NODE_DESCRIPTION_MAP.forEach((k, v) -> ctx.addClass(k, List.of(v.fields)));
        return ctx;
    }

    // Public API

    /**
     * Update the context by adding new classes and field declared in an `add` block
     */
    public void update(AddBlock add) {
        for (var clazz : add.classes) {
            addClass(clazz.name, clazz.fields);
        }
        for (var prefixField : add.fields) {
            addField(prefixField.prefix, prefixField.field);
        }
    }

    /**
     * Update the context by deleting classes and field declared in a `del` block
     */
    public void update(DelBlock del) {
        for (var clazz : del.classes) {
            deleteClass(clazz);
        }
        for (var prefixField : del.fields) {
            deleteField(prefixField.prefix, prefixField.field);
        }
    }

    // Private methods

    /**
     * Add a new class to the context
     * @throws LKQLRuntimeException if the class already exists
     */
    private void addClass(String className, Collection<String> fields) {
        if (env.containsKey(className)) throw LKQLRuntimeException.shouldNotHappen(
            "invalid add class: " + className
        );
        env.put(className, new ClassDescriptor(new HashSet<>(fields)));
    }

    /**
     * Delete a class from the context
     * @throws if context does not contains className
     */
    private void deleteClass(String className) {
        if (env.remove(className) == null) throw LKQLRuntimeException.shouldNotHappen(
            "invalid del class: " + className
        );
    }

    /**
     * Add a new field to an existing class already in the context
     * @throws if context does not contains className
     *         or if className already contains fieldName
     */
    private void addField(String className, String fieldName) {
        final ClassDescriptor existing = env.get(className);
        if (
            existing == null || !existing.fields.add(fieldName)
        ) throw LKQLRuntimeException.shouldNotHappen(
            "invalid add field: " + className + "." + fieldName
        );
    }

    /**
     * Delete a field from a class of the context
     * @throws if context does not contains className
     *            or if className does not contains fieldName
     */
    private void deleteField(String className, String fieldName) {
        final var existing = env.get(className);
        if (
            existing == null || !existing.fields.remove(fieldName)
        ) throw LKQLRuntimeException.shouldNotHappen(
            "invalid del field: " + className + "." + fieldName
        );
    }

    public record ClassDescriptor(HashSet<String> fields) {}
}
