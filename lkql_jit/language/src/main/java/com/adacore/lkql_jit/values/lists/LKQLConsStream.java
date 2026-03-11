//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.lists;

import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.LKQLUnit;
import com.adacore.lkql_jit.values.interop.LKQLCollection;
import com.oracle.truffle.api.CallTarget;

/**
 * This class represents a stream, with a known head and an execution unit to get its tail in a
 * lazy way.
 */
public class LKQLConsStream extends BaseLKQLLazyList {

    // ----- Attributes -----

    /** Instance of "Nil", used to flag the end of a streams. */
    private static final LKQLConsStream NIL = new LKQLConsStream(null, null, null);

    /** Known value of the stream's head. */
    protected final Object head;

    /** Execution unit to get the tail of the stream. */
    private final CallTarget tailExecutionUnit;

    /** Closure for the tail execution unit. */
    private final Closure tailClosure;

    /** Store the value of the tail after its computation. */
    private LKQLConsStream tailCache;

    /** Next stream to get the head from when computing the content of this stream. */
    private LKQLConsStream next;

    // ----- Constructors -----

    public LKQLConsStream(Object head, CallTarget tailExecutionUnit, Closure tailClosure) {
        super(null);
        this.head = head;
        this.tailExecutionUnit = tailExecutionUnit;
        this.tailClosure = tailClosure;
        this.tailCache = null;
        this.next = this;
    }

    // ----- Lazy list required methods -----

    @Override
    protected void initCacheTo(long n) {
        // Ensure the cache is initialized
        if (this.cache == null) {
            this.cache = new ListStorage<>(16);
        }

        // Then compute all required values
        while (this.next.head != null && (n >= this.cache.size() || n < 0)) {
            this.cache.append(this.next.head);
            this.next = this.next.computeTail();
        }
    }

    // ----- Instance methods -----

    protected LKQLConsStream computeTail() {
        if (this.tailCache == null) {
            var tailValue = this.tailExecutionUnit.call(this.tailClosure);
            switch (tailValue) {
                case LKQLConsStream lazyList -> this.tailCache = lazyList;
                case LKQLUnit _ -> this.tailCache = NIL;
                default -> throw LKQLRuntimeError.wrongType(
                    LKQLTypesHelper.LKQL_STREAM,
                    LKQLTypesHelper.fromJava(tailValue),
                    null
                );
            }
        }
        return this.tailCache;
    }

    // ----- Inner classes -----

    /** This is a special stream that is the result of the concatenation of a list and a stream. */
    public static class LKQLComposedStream extends LKQLConsStream {

        public LKQLComposedStream(
            LKQLCollection head,
            CallTarget tailExecutionUnit,
            Closure tailClosure
        ) {
            super(head, tailExecutionUnit, tailClosure);
        }

        @Override
        public Object get(long i) throws IndexOutOfBoundsException {
            LKQLCollection headList = (LKQLCollection) this.head;
            try {
                return headList.get(i);
            } catch (IndexOutOfBoundsException e) {
                return this.computeTail().get(i - headList.size());
            }
        }
    }
}
