//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.exceptions.LKQLEngineException;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.values.LKQLFunction;
import com.adacore.lkql_jit.values.interfaces.Iterable;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;

/** This class represents the result of a mapping operation on a stream. */
public class LKQLMapResult extends BaseCachedStream {

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

    // ----- Instance methods -----

    @Override
    protected void initCacheTo(long n) {
        try {
            while (this.generatorIterator.hasNext() && (n >= this.cache.size() || n < 0)) {
                this.cache.append(
                    this.functionLibrary.execute(
                        this.mappingFunction,
                        this.mappingFunction.closure,
                        this.generatorIterator.next()
                    )
                );
            }
        } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
            throw LKQLEngineException.create(e);
        }
    }
}
