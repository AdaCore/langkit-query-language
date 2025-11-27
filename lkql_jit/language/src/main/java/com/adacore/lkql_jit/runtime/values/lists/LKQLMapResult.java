//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values.lists;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterator;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;

/** This class represents the result of a mapping operation on a lazy list. */
public class LKQLMapResult extends BaseLKQLLazyList {

    // ----- Attributes -----

    /** Collection that is mapped. */
    private final Iterator generatorIterator;

    /** Function to perform the mapping. */
    private final LKQLFunction mappingFunction;

    /** Interop library used to call the mapping function. */
    private final InteropLibrary functionLibrary;

    // ----- Constructors -----

    public LKQLMapResult(
        Iterable generator,
        LKQLFunction mappingFunction,
        InteropLibrary functionLibrary
    ) {
        super(new ListStorage<>(16));
        this.generatorIterator = generator.iterator();
        this.mappingFunction = mappingFunction;
        this.functionLibrary = functionLibrary;
    }

    // ----- Lazy list required methods -----

    @Override
    protected void initCacheTo(long n) {
        try {
            while (this.generatorIterator.hasNext() && (n >= this.cache.size() || n < 0)) {
                this.cache.append(
                        this.functionLibrary.execute(
                                this.mappingFunction,
                                this.mappingFunction.closure.getContent(),
                                this.generatorIterator.next()
                            )
                    );
            }
        } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
            throw LKQLRuntimeException.fromJavaException(e, null);
        }
    }
}
