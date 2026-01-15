//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values;

import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.values.interop.LKQLCallable;
import com.adacore.lkql_jit.values.lists.LKQLSelectorList;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;

/** This class represents the selector values in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLSelector extends LKQLCallable {

    // ----- Attributes -----

    /** String array corresponding to parameters expected by a selector. */
    private static final String[] SELECTOR_PARAMETERS = { "root" };

    /** Node array containing default value for selector parameters. */
    private static final Node[] SELECTOR_DEFAULT_PARAMETERS = { null };

    /** The root node containing the selector semantics. */
    private final RootNode rootNode;

    /** Closure for the selector execution. */
    private final Closure closure;

    private final boolean checkCycles;

    // ----- Constructors -----

    /**
     * Create a new selector value.
     *
     * @param rootNode The root node of the selector.
     * @param closure The closure of the selector.
     * @param name The name of the selector.
     * @param documentation The documentation of the selector.
     */
    public LKQLSelector(
        RootNode rootNode,
        Closure closure,
        String name,
        String documentation,
        boolean checkCycles
    ) {
        super(
            rootNode.getName(),
            LKQLCallable.CallableKind.SELECTOR,
            SELECTOR_PARAMETERS,
            SELECTOR_DEFAULT_PARAMETERS,
            documentation
        );
        this.rootNode = rootNode;
        this.closure = closure;
        this.checkCycles = checkCycles;
    }

    // ----- Instance functions -----

    /** Execute the selector value. */
    public LKQLSelectorList getList(Object value) {
        return this.getList(value, -1, -1, -1);
    }

    /**
     * Execute the selector value on an ada node with additional arguments.
     *
     * @param maxDepth The maximum depth of the selector list.
     * @param minDepth The minimal depth of the selector list.
     * @param depth The precise depth to get.
     */
    public LKQLSelectorList getList(Object value, int maxDepth, int minDepth, int depth) {
        return new LKQLSelectorList(
            this.rootNode,
            this.closure,
            value,
            maxDepth,
            minDepth,
            depth,
            checkCycles
        );
    }

    // ----- Value methods -----

    @ExportMessage
    public Object execute(Object[] arguments) throws ArityException {
        if (arguments.length != 1) throw ArityException.create(1, 1, arguments.length);
        return this.getList(arguments[0]);
    }

    // ----- LKQL value methods -----

    @CompilerDirectives.TruffleBoundary
    public String lkqlProfile() {
        return this.name + "()";
    }

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof LKQLSelector other)) return false;
        return ObjectUtils.equals(rootNode, other.rootNode);
    }

    @Override
    public int hashCode() {
        return ObjectUtils.hashCode(rootNode);
    }
}
