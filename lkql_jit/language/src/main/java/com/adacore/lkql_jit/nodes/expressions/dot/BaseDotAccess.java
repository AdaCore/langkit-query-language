//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.built_ins.BuiltInMethodValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Map;

/**
 * This node is the base of all dot access nodes of the LKQL engine, it contains all common
 * operations and execution paths.
 */
@NodeChild(value = "receiver", type = Expr.class)
public abstract class BaseDotAccess extends Expr {

    // ----- Attributes -----

    /** The member to access. */
    protected final Identifier member;

    // ----- Constructors -----

    public BaseDotAccess(SourceSection location, Identifier member) {
        super(location);
        this.member = member;
    }

    // ----- Getters -----

    public abstract Expr getReceiver();

    // ----- Instance methods -----

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
