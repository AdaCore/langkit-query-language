//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.values.streams;

import com.adacore.lkql_jit.exceptions.LKQLRuntimeError;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.ListStorage;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.values.interfaces.Indexable;
import com.adacore.lkql_jit.values.interfaces.Iterator;
import com.adacore.lkql_jit.values.interop.LKQLStream;
import com.adacore.lkql_jit.values.iterators.LKQLStreamIterator;
import com.oracle.truffle.api.CallTarget;

/**
 * This class represents a stream, with a known head and an execution unit to get its tail in a
 * lazy way.
 */
public class LKQLConsStream extends BaseCachedStream {

    // ----- Attributes -----

    /** Next stream to get the head from when computing the content of this stream. */
    private LKQLStream next;

    // ----- Constructors -----

    private LKQLConsStream(LKQLStream wrapped) {
        super(new ListStorage<>(16));
        this.next = wrapped;
    }

    public static LKQLConsStream concatStream(
        Indexable prefix,
        CallTarget tailExecutionUnit,
        Closure tailClosure
    ) {
        return new LKQLConsStream(new RawConcatStream(prefix, tailExecutionUnit, tailClosure));
    }

    public static LKQLConsStream consStream(
        Object head,
        CallTarget tailExecutionUnit,
        Closure tailClosure
    ) {
        return new LKQLConsStream(new RawConsStream(head, tailExecutionUnit, tailClosure));
    }

    // ----- Instance methods -----

    @Override
    protected void initCacheTo(long n) {
        while (this.next != null && (n >= this.cache.size() || n < 0)) {
            this.cache.append(this.next.getHead());
            this.next = this.next.getTail();
        }
    }

    // ----- Inner classes -----

    /** Uncached stream, should be wrapped by LKQLConsStream. */
    private static class RawConsStream extends LKQLStream {

        /** Known value of the stream's head. */
        private final Object head;

        /** Execution unit to get the tail of the stream. */
        private final CallTarget tailExecutionUnit;
        /** Closure for the tail execution unit. */
        private final Closure tailClosure;

        RawConsStream(Object head, CallTarget tailExecutionUnit, Closure tailClosure) {
            this.head = head;
            this.tailExecutionUnit = tailExecutionUnit;
            this.tailClosure = tailClosure;
        }

        /** Very inefficient, should not be used. */
        public Iterator iterator() {
            return new LKQLStreamIterator(this);
        }

        /** Very inefficient, should not be used. */
        public Object get(long index) throws IndexOutOfBoundsException {
            if (index == 0) return getHead();
            else return getTail().get(index - 1);
        }

        /** Should be called once and cached by wrapper. */
        public Object getHead() {
            return this.head;
        }

        /** Should be called once and cached by wrapper. */
        public LKQLStream getTail() {
            var executionResult = this.tailExecutionUnit.call(this.tailClosure);
            return switch (executionResult) {
                case LKQLConsStream consTail -> consTail.next;
                case LKQLStream tail -> tail;
                default -> throw LKQLRuntimeError.wrongType(
                    LKQLTypesHelper.LKQL_STREAM,
                    LKQLTypesHelper.fromJava(executionResult),
                    null
                );
            };
        }
    }

    /** Uncached stream, should be wrapped by LKQLConsStream. */
    private static class RawConcatStream extends LKQLStream {

        /** Prefix of the stream. */
        private final Indexable prefix;
        /** Where the stream is with respect to the prefix. */
        private final long index;

        /** Execution unit to get the tail of the stream. */
        private final CallTarget tailExecutionUnit;
        /** Closure for the tail execution unit. */
        private final Closure tailClosure;

        /** Prevent tail from being computed multiple times. */
        private Object executionResult = null;

        public RawConcatStream(
            Indexable prefix,
            CallTarget tailExecutionUnit,
            Closure tailClosure
        ) {
            this.prefix = prefix;
            this.index = 0;
            this.tailExecutionUnit = tailExecutionUnit;
            this.tailClosure = tailClosure;
        }

        private RawConcatStream(
            Indexable prefix,
            long index,
            CallTarget tailExecutionUnit,
            Closure tailClosure
        ) {
            this.index = index;
            this.prefix = prefix;
            this.tailExecutionUnit = tailExecutionUnit;
            this.tailClosure = tailClosure;
        }

        /** Very inefficient, should not be used. */
        public Iterator iterator() {
            return new LKQLStreamIterator(this);
        }

        /** Very inefficient, should not be used. */
        public Object get(long index) throws IndexOutOfBoundsException {
            if (index == 0) return getHead();
            else return getTail().get(index - 1);
        }

        /** Should be called once and cached by wrapper. */
        public Object getHead() {
            try {
                // try to access element in prefix first
                return this.prefix.get(index);
            } catch (Exception _) {
                // end of prefix, continue with tail
                return this.getTail().getHead();
            }
        }

        /** Should be called once and cached by wrapper. */
        public LKQLStream getTail() {
            try {
                // try to access element in prefix first
                this.prefix.get(index + 1);
                return new RawConcatStream(prefix, index + 1, tailExecutionUnit, tailClosure);
            } catch (Exception _) {
                // end of prefix, continue with tail
                if (executionResult == null) executionResult = this.tailExecutionUnit.call(
                    this.tailClosure
                );
                return switch (executionResult) {
                    case LKQLConsStream consTail -> consTail.next;
                    case LKQLStream tail -> tail;
                    default -> throw LKQLRuntimeError.wrongType(
                        LKQLTypesHelper.LKQL_STREAM,
                        LKQLTypesHelper.fromJava(executionResult),
                        null
                    );
                };
            }
        }
    }
}
