//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.values;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.ReflectionUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.utilities.TriState;

/** This class represents a Libadalang property access in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLProperty extends BasicLKQLValue {

    // ----- Attributes -----

    /** The name of the Libadalang property. */
    private final String name;

    /** Description of the Libadalang property with its Java method and parameters. */
    private final Libadalang.Reflection.Field description;

    /** The node associated to the property. */
    private final Libadalang.AdaNode node;

    // ----- Constructors -----

    /** Create a new LKQL property from its name and associated node. */
    public LKQLProperty(final String name, final Libadalang.AdaNode node) {
        this.name = name;
        this.description = node.getFieldDescription(name);
        this.node = node;
    }

    /** Creation function used by the Truffle DSL to cached properties */
    public static LKQLProperty create(final String name, final Libadalang.AdaNode node) {
        return new LKQLProperty(name, node);
    }

    // ----- Getters -----

    public Libadalang.AdaNode getNode() {
        return node;
    }

    public Libadalang.Reflection.Field getDescription() {
        return description;
    }

    // ----- Instance methods -----

    /** Get whether the property reference point to a node field. */
    public boolean isField() {
        return this.name.startsWith("f");
    }

    /**
     * Execute the property with the given arguments.
     *
     * @param caller The locatable which called the execution.
     * @param arguments The argument for the property call.
     */
    public Object executeAsProperty(Node caller, ArgList argList, Object... arguments) {
        try {
            return ReflectionUtils.callProperty(
                    this.node, this.description, caller, argList, arguments);
        } catch (com.adacore.lkql_jit.exception.utils.UnsupportedTypeException e) {
            throw LKQLRuntimeException.unsupportedType(
                    LKQLTypesHelper.category(e.getType()), caller);
        }
    }

    /**
     * Execute the property as a field access without arguments.
     *
     * @param caller The locatable which called the execution.
     */
    public Object executeAsField(Node caller) {
        try {
            return ReflectionUtils.callProperty(this.node, this.description, caller, null);
        } catch (com.adacore.lkql_jit.exception.utils.UnsupportedTypeException e) {
            throw LKQLRuntimeException.unsupportedType(
                    LKQLTypesHelper.category(e.getType()), caller);
        }
    }

    // ----- Value methods -----

    /** Exported message to compare two LKQL properties. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {
        /** Compare two LKQL properties. */
        @Specialization
        protected static TriState onProperty(final LKQLProperty left, final LKQLProperty right) {
            return TriState.valueOf(ObjectUtils.equals(left.description, right.description));
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
                @SuppressWarnings("unused") final LKQLProperty receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL property. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public static int identityHashCode(final LKQLProperty receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @Override
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return "property<" + this.name + ">";
    }

    /** Tell the interop library that the value is not executable. */
    @ExportMessage
    public boolean isExecutable() {
        // TODO (issue #143): Make the LKQLSelector executable as LKQLFunctions
        return false;
    }

    /** Placeholder function for the Truffle DSL. */
    @ExportMessage
    public Object execute(Object[] arguments)
            throws UnsupportedTypeException, ArityException, UnsupportedMessageException {
        // TODO (issue #143): implement this method to execute the property as a simple function
        return null;
    }
}
