/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.values.LKQLNull;
import com.adacore.lkql_jit.built_ins.values.LKQLProperty;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.expressions.Expr;
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
    protected SafeDotAccess(SourceLocation location, Identifier member) {
        super(location);
        this.member = member;
    }

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
     * Execute the safe dot access on a node with the un-cached strategy.
     *
     * @param receiver The node receiver.
     * @return The property reference or the field value.
     */
    @Specialization(replaces = "onNodeCached")
    protected Object onNodeUncached(Libadalang.AdaNode receiver) {
        // Try the built_in
        Object builtIn = this.tryBuildIn(receiver);
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
            throw LKQLRuntimeException.noSuchField(this.member);
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

        // Return the default null
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
