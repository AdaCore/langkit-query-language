//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns.node_patterns;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.patterns.BasePattern;
import com.adacore.lkql_jit.runtime.values.LKQLProperty;
import com.adacore.lkql_jit.utils.functions.ReflectionUtils;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a pattern detail on a property in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
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
    protected BasePattern expected;

    // ----- Constructors -----

    /**
     * Create a new pattern detail on a property.
     */
    public NodePatternProperty(
        SourceSection location,
        String propertyName,
        Expr[] args,
        BasePattern expected
    ) {
        super(location);
        this.propertyName = propertyName;
        this.args = args;
        this.expected = expected;
    }

    // ----- Execution methods -----

    /**
     * Execute the property detail with the cached path.
     *
     * @param frame The frame to execute in.
     * @param node The node get the property from.
     * @param property The cached property reference.
     * @return True if the detail is valid, false else.
     */
    @Specialization(guards = { "node == property.node", "property.description != null" })
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

        // Get the property result
        Object result;
        try {
            result = ReflectionUtils.callProperty(
                property.node,
                property.description,
                this,
                args,
                arguments
            );
        } catch (com.adacore.lkql_jit.exception.utils.UnsupportedTypeException e) {
            throw LKQLRuntimeException.unsupportedType(e.getType(), this);
        }
        Object value = result;

        // Verify the pattern
        return this.expected.executeValue(frame, value);
    }

    /**
     * Execute the property detail with the un-cached path.
     *
     * @param frame The frame to execute in.
     * @param node The node get the property from.
     * @return True if the detail is valid, false else.
     */
    @Specialization(replaces = "propertyCached")
    protected boolean propertyUncached(VirtualFrame frame, LangkitSupport.NodeInterface node) {
        // Get the property methods
        LKQLProperty property = new LKQLProperty(this.propertyName, node);

        // Test if the property is null
        if (property.description == null) {
            throw LKQLRuntimeException.noSuchField(this);
        }

        // Return the result
        return this.propertyCached(frame, node, property);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "propertyName" },
                new Object[] { this.propertyName }
            );
    }
}
