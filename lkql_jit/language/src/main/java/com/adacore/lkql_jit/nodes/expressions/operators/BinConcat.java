//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.expressions.operators;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.DummyLocation;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import java.util.Arrays;

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
     * @param leftLocation The location of the left node.
     * @param rightLocation The location of the right node.
     */
    protected BinConcat(
            SourceLocation location, DummyLocation leftLocation, DummyLocation rightLocation) {
        super(location, leftLocation, rightLocation);
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
            final Object[] resContent = Arrays.copyOf(left.content, leftSize + rightSize);
            System.arraycopy(right.content, 0, resContent, leftSize, rightSize);
            return new LKQLList(resContent);
        } catch (UnsupportedMessageException e) {
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
