//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.langkit_support.LangkitSupport.Reflection.Param;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.ReflectionUtils;
import com.adacore.lkql_jit.values.interop.LKQLCallable;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;

/** This class represents a Libadalang property access in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLProperty extends LKQLCallable {

    // ----- Attributes -----

    /** Description of the property with its Java method and parameters. */
    public final LangkitSupport.Reflection.Field description;

    /** The node associated to the property. */
    public final LangkitSupport.NodeInterface node;

    // ----- Constructors -----

    private LKQLProperty(
        String name,
        LangkitSupport.Reflection.Field description,
        LangkitSupport.NodeInterface node
    ) {
        super(
            name,
            LKQLCallable.CallableKind.PROPERTY,
            description == null
                ? new String[0]
                : description.getParams().stream().map(Param::getName).toArray(v -> new String[v]),
            description == null ? new Node[0] : new Node[description.getParams().size()],
            ""
        );
        this.description = description;
        this.node = node;
    }

    /** Create a new LKQL property from its name and associated node. */
    @CompilerDirectives.TruffleBoundary
    public LKQLProperty(final String name, final LangkitSupport.NodeInterface node) {
        this(name, node.getFieldDescription(name), node);
    }

    /** Creation function used by the Truffle DSL to cached properties */
    public static LKQLProperty create(final String name, final LangkitSupport.NodeInterface node) {
        return new LKQLProperty(name, node);
    }

    // ----- Instance methods -----

    /** Get whether the property reference point to a node field. */
    public boolean isField() {
        return this.name.startsWith("f");
    }

    /**
     * Execute the property as a field access without arguments.
     *
     * @param caller The locatable which called the execution.
     */
    @CompilerDirectives.TruffleBoundary
    public Object executeAsField(Node caller) {
        return ReflectionUtils.callProperty(this.node, this.description, caller, null);
    }

    // ----- Value methods -----

    @ExportMessage
    public Object execute(Object[] arguments)
        throws UnsupportedTypeException, ArityException, UnsupportedMessageException {
        return ReflectionUtils.callProperty(this.node, this.description, null, null, arguments);
    }

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LKQLProperty other)) return false;
        return ObjectUtils.equals(this.description, other.description);
    }

    @Override
    public int hashCode() {
        return ObjectUtils.hashCode(this.description);
    }
}
