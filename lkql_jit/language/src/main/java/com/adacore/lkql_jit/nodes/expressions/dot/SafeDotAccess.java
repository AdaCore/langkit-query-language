//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.built_ins.BuiltInMethodValue;
import com.adacore.lkql_jit.built_ins.values.LKQLNull;
import com.adacore.lkql_jit.built_ins.values.LKQLProperty;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Map;

/**
 * This node represents the safe dot access in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@NodeChild(value = "receiver", type = Expr.class)
public abstract class SafeDotAccess extends Expr {

    // ----- Attributes -----

    /** The member to access. */
    protected final Identifier member;

    // ----- Constructors -----

    /**
     * Create a new base dot access with the needed parameters.
     *
     * @param location The location of the dot access.
     * @param member The member to access.
     */
    protected SafeDotAccess(SourceSection location, Identifier member) {
        super(location);
        this.member = member;
    }

    public abstract Expr getReceiver();

    // ----- Execution methods -----

    /**
     * Execute the safe dot access on a node with the cached strategy.
     *
     * @param receiver The node receiver.
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
     * Execute the safe dot access on a node with the un-cached strategy.
     *
     * @param receiver The node receiver.
     * @return The property reference or the field value.
     */
    @Specialization(replaces = "onNodeCached")
    protected Object onNodeUncached(Libadalang.AdaNode receiver) {
        // Try the built_in
        Object builtIn = this.tryBuiltIn(receiver);
        if (builtIn != null) {
            return builtIn;
        }

        // Test if the receiver is null
        if (receiver == LKQLNull.INSTANCE) {
            return LKQLNull.INSTANCE;
        }

        // Create the property reference
        LKQLProperty propertyRef = new LKQLProperty(this.member.getName(), receiver);
        if (propertyRef.getDescription() == null) {
            throw LKQLRuntimeException.noSuchField(this.getReceiver());
        }

        // Return the result
        return this.onNodeCached(receiver, propertyRef, propertyRef.isField());
    }

    /**
     * Fallback when the receiver is a generic object.
     *
     * @param receiver The receiver generic value.
     */
    @Fallback
    protected void onGeneric(Object receiver) {
        throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.ADA_NODE, LKQLTypesHelper.fromJava(receiver), this);
    }

    // ----- Class methods -----

    /**
     * Try to get a built-in method and execute it if there is no parameters.
     *
     * @param receiver The receiver object.
     * @return The built-in result, null if the built-in method doesn't exist.
     */
    protected Object tryBuiltIn(Object receiver) {
        // Get the built in
        BuiltInMethodValue builtinMethod = this.getBuiltIn(receiver);
        if (builtinMethod != null) {
            if (builtinMethod.parameterNames.length <= 1) {
                InteropLibrary builtInLibrary = InteropLibrary.getUncached(builtinMethod);
                try {
                    return builtInLibrary.execute(builtinMethod, builtinMethod.thisValue);
                } catch (ArityException
                        | UnsupportedTypeException
                        | UnsupportedMessageException e) {
                    throw LKQLRuntimeException.fromJavaException(e, this);
                }
            } else {
                return builtinMethod;
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
    protected BuiltInMethodValue getBuiltIn(Object receiver) {
        // Get the LKQL context
        LKQLContext context = LKQLLanguage.getContext(this);
        Map<String, BuiltInMethodFactory> metaTable =
                context.getMetaTable(LKQLTypesHelper.fromJava(receiver));

        // Return the built-in method or null
        if (metaTable.containsKey(this.member.getName())) {
            return metaTable.get(this.member.getName()).instantiate(receiver);
        } else {
            return null;
        }
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
