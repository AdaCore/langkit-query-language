//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLProperty;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents the safe dot access in the LKQL language. */
public abstract class SafeDotAccess extends BaseDotAccess {

    // ----- Constructors -----

    /** Create a new sage dot access. */
    protected SafeDotAccess(SourceSection location, Identifier member) {
        super(location, member);
    }

    // ----- Execution methods -----

    /**
     * Execute the safe dot access on a node with the cached strategy.
     *
     * @param property The cached property reference.
     * @param isField The cached value if the property is a field.
     * @return The property reference or the field value.
     */
    @Specialization(
        guards = {
            "!receiver.isNone()",
            "getBuiltIn(receiver) == null",
            "receiver == property.node",
            "property.description != null",
        },
        limit = "1"
    )
    protected Object onNodeCached(
        @SuppressWarnings("unused") LangkitSupport.NodeInterface receiver,
        @Cached("create(member.getName(), receiver)") LKQLProperty property,
        @Cached("property.isField()") boolean isField
    ) {
        // If the method is a field
        if (isField) {
            return property.executeAsField(this);
        }
        // If the method is a property
        else {
            return property;
        }
    }

    /**
     * Execute the safe dot access on a node with the un-cached strategy.
     *
     * @return The property reference or the field value.
     */
    @Specialization(replaces = "onNodeCached")
    protected Object onNodeUncached(LangkitSupport.NodeInterface receiver) {
        // Try the built_in
        Object builtIn = this.getBuiltIn(receiver);
        if (builtIn != null) {
            return builtIn;
        }

        // Test if the receiver is null
        if (receiver == LKQLNull.INSTANCE) {
            return LKQLNull.INSTANCE;
        }

        // Create the property reference
        LKQLProperty propertyRef = new LKQLProperty(this.member.getName(), receiver);
        if (propertyRef.description == null) {
            throw LKQLRuntimeException.noSuchField(this.getReceiver());
        }

        // Return the result
        return this.onNodeCached(receiver, propertyRef, propertyRef.isField());
    }

    /** Fallback when the receiver is a generic object. */
    @Fallback
    protected void onGeneric(Object receiver) {
        throw LKQLRuntimeException.wrongType(
            LKQLTypesHelper.NODE_INTERFACE,
            LKQLTypesHelper.fromJava(receiver),
            this
        );
    }
}
