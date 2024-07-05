//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.built_ins.values.*;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.source.SourceSection;

/** This node represents the dot access in the LKQL language. */
public abstract class DotAccess extends BaseDotAccess {

    // ----- Constructors -----

    /** Create a new dot access. */
    protected DotAccess(SourceSection location, Identifier member) {
        super(location, member);
    }

    // ----- Execution methods -----

    /** Access to a member of an object value. */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onObject(
            final LKQLObject receiver,
            @CachedLibrary("receiver") DynamicObjectLibrary receiverLibrary) {
        // Try to get the built in
        Object builtIn = this.getBuiltIn(receiver);
        if (builtIn != null) {
            return builtIn;
        }

        // Get the object member
        final Object res = receiverLibrary.getOrDefault(receiver, this.member.getName(), null);
        if (res != null) {
            return res;
        }

        // Throw an exception if the member was not found
        throw LKQLRuntimeException.noSuchMember(this);
    }

    /** Execute the dot access on a namespace value. */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onNamespace(
            final LKQLNamespace receiver,
            @CachedLibrary("receiver") DynamicObjectLibrary receiverLibrary) {
        // Try to get the built in
        Object builtIn = this.getBuiltIn(receiver);
        if (builtIn != null) {
            return builtIn;
        }

        // Get the namespace member
        Object res = receiverLibrary.getOrDefault(receiver, this.member.getName(), null);
        if (res != null) {
            return res;
        }

        // Throw an exception if the member was n<w>t found
        throw LKQLRuntimeException.noSuchMember(this);
    }

    /**
     * Execute the dot access on a node with the cached strategy.
     *
     * @param property The cached property reference.
     * @param isField The cached value if the property is a field.
     * @return The property reference or the field value.
     */
    @Specialization(
            guards = {
                "!receiver.isNone()",
                "getBuiltIn(receiver) == null",
                "receiver == property.getNode()",
                "property.getDescription() != null"
            },
            limit = "1")
    protected Object onNodeCached(
            @SuppressWarnings("unused") Libadalang.AdaNode receiver,
            @Cached("create(member.getName(), receiver)") LKQLProperty property,
            @Cached("property.isField()") boolean isField) {
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
     * Execute the dot access on a node with the un-cached strategy.
     *
     * @return The property call or the file value.
     */
    @Specialization(replaces = "onNodeCached")
    protected Object onNodeUncached(Libadalang.AdaNode receiver) {
        // Try the built_in
        Object builtIn = this.getBuiltIn(receiver);
        if (builtIn != null) {
            return builtIn;
        }

        // Test if the node is null
        if (receiver == LKQLNull.INSTANCE) {
            throw LKQLRuntimeException.nullReceiver(this);
        }

        // Create the property reference
        LKQLProperty property = new LKQLProperty(this.member.getName(), receiver);
        if (property.getDescription() == null) {
            throw LKQLRuntimeException.noSuchField(this.member);
        }

        // Return the result
        return this.onNodeCached(receiver, property, property.isField());
    }

    /** Fallback when the receiver is none of the case identified by the specializations. */
    @Fallback
    protected Object onOthers(Object receiver) {
        // In the fallback case, only built-in methods are candidates. Try to get a built-in.
        Object builtIn = this.getBuiltIn(receiver);
        if (builtIn != null) {
            return builtIn;
        }

        throw LKQLRuntimeException.wrongMember(
                this.member.getName(), LKQLTypesHelper.fromJava(receiver), this.member);
    }
}
