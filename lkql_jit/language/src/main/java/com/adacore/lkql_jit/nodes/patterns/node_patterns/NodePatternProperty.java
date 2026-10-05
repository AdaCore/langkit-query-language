//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.Constants;
import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.Pattern;
import com.adacore.lkql_jit.utils.functions.ReflectionUtils;
import com.adacore.lkql_jit.values.AdaNodeProxy;
import com.adacore.lkql_jit.values.LKQLProperty;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents a pattern detail on a property. */
public abstract class NodePatternProperty extends NodePatternDetail {

    // ----- Attributes -----

    /** The name of the property to call. */
    protected final String propertyName;

    // ----- Children -----

    /** The list of the argument for the property call. */
    @Children
    @SuppressWarnings("FieldMayBeFinal")
    protected Expr[] args;

    /** The expected value for the property call. */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    protected Pattern expected;

    // ----- Constructors -----

    /**
     * Create a new pattern detail on a property.
     *
     * @param location The location of the node in the source.
     */
    public NodePatternProperty(
        SourceSection location,
        String propertyName,
        Expr[] args,
        Pattern expected
    ) {
        super(location);
        this.propertyName = propertyName;
        this.args = args;
        this.expected = expected;
    }

    // ----- Execution methods -----

    /** Execute the property detail with the cached path. */
    @Specialization(
        guards = { "node == property.node", "property.description != null" },
        limit = Constants.SPECIALIZED_LIB_LIMIT
    )
    @ExplodeLoop
    protected boolean propertyCached(
        VirtualFrame frame,
        @SuppressWarnings("unused") LangkitSupport.NodeInterface node,
        @Cached("create(propertyName, node)") LKQLProperty property
    ) {
        // Evaluate the arguments
        Object[] arguments = new Object[args.length];
        for (int i = 0; i < arguments.length; i++) {
            arguments[i] = args[i].executeGeneric(frame);
        }

        // Verify the pattern
        return this.expected.executeValue(
            frame,
            ReflectionUtils.callProperty(property.node, property.description, this, args, arguments)
        );
    }

    /** Execute the property detail with the un-cached path. */
    @Specialization(replaces = "propertyCached")
    protected boolean propertyUncached(VirtualFrame frame, LangkitSupport.NodeInterface node) {
        // Get the property methods
        LKQLProperty property = new LKQLProperty(this.propertyName, node);

        // Test if the property is null
        if (property.description == null) {
            throw LKQLRuntimeError.noSuchField(this);
        }

        // Return the result
        return this.propertyCached(frame, node, property);
    }

    /** Execute the property detail on a dynamic node. */
    @Specialization
    protected boolean propertyAdaNodeProxy(VirtualFrame frame, AdaNodeProxy node) {
        return this.propertyUncached(frame, node.base);
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[] { "propertyName" },
            new Object[] { this.propertyName }
        );
    }
}
