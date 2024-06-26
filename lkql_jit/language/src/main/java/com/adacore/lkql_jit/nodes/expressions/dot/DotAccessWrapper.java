//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.dot;

import com.adacore.lkql_jit.built_ins.BuiltInAttributeValue;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.Constants;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node is used to wrap all dot access nodes and perform eventual post access processing. For
 * example, when accessing a built-in attribute, the value returned by the dot-access node is a
 * method reference which should be implicitly executed by this wrapper to stick to the LKQL
 * semantics.
 */
@NodeChild(value = "dotAccess", type = BaseDotAccess.class)
public abstract class DotAccessWrapper extends Expr {

    // ----- Constructors -----

    public DotAccessWrapper(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * When the child dot-access is returning an attribute, execute this attribute implicitly and
     * return the result of this execution.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected Object onAttribute(
            BuiltInAttributeValue attribute,
            @CachedLibrary("attribute") InteropLibrary attributeLibrary) {
        try {
            return attributeLibrary.execute(attribute, attribute.thisValue);
        } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
            throw LKQLRuntimeException.fromJavaException(e, this);
        }
    }

    /** For other cases, just return the dot access result value. */
    @Fallback
    protected Object onOther(Object other) {
        return other;
    }

    // ----- Override methods -----

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
