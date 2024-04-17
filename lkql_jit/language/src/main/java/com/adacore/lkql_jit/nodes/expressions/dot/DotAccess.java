//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.values.*;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import java.util.Map;

/**
 * This node represents the dot access in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "receiver", type = Expr.class)
public abstract class DotAccess extends Expr {

    // ----- Attributes -----

    /** The member to access. */
    protected final Identifier member;

    // ----- Constructors -----

    /**
     * Create a new base dot access with the needed parameters.
     *
     * @param location The location of the node in the source.
     * @param member The member to access.
     */
    protected DotAccess(SourceLocation location, Identifier member) {
        super(location);
        this.member = member;
    }

    // ----- Execution methods -----

    /**
     * Access to a member of an object value.
     *
     * @param receiver The receiver object value.
     * @return The member of the object.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onObject(
            final LKQLObject receiver,
            @CachedLibrary("receiver") DynamicObjectLibrary receiverLibrary) {
        // Try to get the built in
        Object builtIn = this.tryBuildIn(receiver);
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

    /**
     * Execute the dot access on a namespace value.
     *
     * @param receiver The namespace.
     * @return The member of the namespace.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onNamespace(
            final LKQLNamespace receiver,
            @CachedLibrary("receiver") DynamicObjectLibrary receiverLibrary) {
        // Try to get the built in
        Object builtIn = this.tryBuildIn(receiver);
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
     * @param receiver The node receiver.
     * @param property The cached property reference.
     * @param isField The cached value if the property is a field.
     * @return The result of the property reference.
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
            Libadalang.AdaNode receiver,
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
     * @param receiver The node receiver.
     * @return The result of the property call.
     */
    @Specialization(replaces = "onNodeCached")
    protected Object onNodeUncached(Libadalang.AdaNode receiver) {
        // Try the built_in
        Object builtIn = this.tryBuildIn(receiver);
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

    /**
     * Fallback when the receiver is a generic object.
     *
     * @param receiver The receiver generic value.
     * @return The execution of the dot access.
     */
    @Fallback
    protected Object onGeneric(Object receiver) {
        // Try to get the built in
        Object builtIn = this.tryBuildIn(receiver);
        if (builtIn != null) {
            return builtIn;
        }

        // Throw an exception
        throw LKQLRuntimeException.wrongMember(
                this.member.getName(), LKQLTypesHelper.fromJava(receiver), this.member);
    }

    // ----- Class methods -----

    /**
     * Try to get a built-in method and execute it if there is no parameters.
     *
     * @param receiver The receiver object.
     * @return The built-in result, null if the built-in method doesn't exist.
     */
    protected Object tryBuildIn(Object receiver) {
        // Get the built in
        BuiltInFunctionValue builtIn = this.getBuiltIn(receiver);
        if (builtIn != null) {
            InteropLibrary builtInLibrary = InteropLibrary.getUncached(builtIn);
            if (builtIn.parameterNames.length <= 1) {
                try {
                    return builtInLibrary.execute(builtIn, receiver);
                } catch (ArityException
                        | UnsupportedTypeException
                        | UnsupportedMessageException e) {
                    // TODO: Implement runtime checks in the LKQLFunction class and base computing
                    // on them (#138)
                    throw LKQLRuntimeException.fromJavaException(e, this.member);
                }
            } else {
                builtIn.setThisValue(receiver);
                return builtIn;
            }
        }

        // Return the null value if there is no method
        return null;
    }

    /**
     * Get the built-in method for the receiver.
     *
     * @param receiver The receiver object.
     * @return The member if it exists, null else.
     */
    @CompilerDirectives.TruffleBoundary
    protected BuiltInFunctionValue getBuiltIn(Object receiver) {
        // Get the LKQL context
        LKQLContext context = LKQLLanguage.getContext(this);
        Map<String, BuiltInFunctionValue> metaTable =
                context.getMetaTable(LKQLTypesHelper.fromJava(receiver));

        // Return the built-in method or null
        return metaTable.getOrDefault(this.member.getName(), null);
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"member"}, new Object[] {this.member});
    }
}
