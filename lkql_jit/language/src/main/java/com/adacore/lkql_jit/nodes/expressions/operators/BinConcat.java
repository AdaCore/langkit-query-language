//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ListUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;
import java.util.List;

/**
 * This node represents the concatenation operation in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class BinConcat extends BinOp {

    // ----- Constructors -----

    /**
     * Create a concatenation node.
     *
     * @param location The location of the node in the source.
     */
    protected BinConcat(SourceSection location) {
        super(location);
    }

    // ----- Execution methods -----

    /**
     * Concatenate two strings.
     *
     * @param left The left string value.
     * @param right The right string value.
     * @return The result of the string concatenation.
     */
    @Specialization
    protected String concatStrings(String left, String right) {
        return StringUtils.concat(left, right);
    }

    /**
     * Concatenate two lists.
     *
     * @param left The left list value.
     * @param right The right list value.
     * @return The result of the list concatenation.
     */
    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected LKQLList concatLists(
            final LKQLList left,
            final LKQLList right,
            @CachedLibrary("left") InteropLibrary leftLibrary,
            @CachedLibrary("right") InteropLibrary rightLibrary) {
        try {
            final int leftSize = (int) leftLibrary.getArraySize(left);
            final int rightSize = (int) rightLibrary.getArraySize(right);
            List<Object> resContent = new ArrayList<>(leftSize + rightSize);
            ListUtils.addAll(resContent, left.content);
            ListUtils.addAll(resContent, right.content);
            return new LKQLList(resContent.toArray(new Object[0]));
        } catch (Exception e) {
            throw LKQLRuntimeException.fromJavaException(e, this);
        }
    }

    /**
     * The fallback method if the concatenation is not applied to correct types.
     *
     * @param left The left value.
     * @param right The right value.
     */
    @Fallback
    protected void nonConcatenable(Object left, Object right) {
        if (LKQLTypeSystemGen.isString(left) || LKQLTypeSystemGen.isLKQLList(left)) {
            throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.fromJava(left), LKQLTypesHelper.fromJava(right), this);
        } else {
            throw LKQLRuntimeException.unsupportedOperation(
                    LKQLTypesHelper.fromJava(left), "&", LKQLTypesHelper.fromJava(right), this);
        }
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel);
    }
}
