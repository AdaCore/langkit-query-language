/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.runtime.values.interfaces;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;
import com.adacore.lkql_jit.runtime.values.NamespaceValue;
import com.adacore.lkql_jit.utils.util_classes.Iterator;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


/**
 * This abstract class represents the base of all lazy collection in the LKQL JIT
 *
 * @author Hugo GUERRIER
 */
public abstract class LazyCollection implements Indexable, Iterable, Truthy {

    // ----- Attributes -----

    /** The namespace for the lazy collection execution */
    protected NamespaceValue namespace;

    /** The cache of the lazy collection */
    protected final List<Object> cache;

    // ----- Constructors -----

    /**
     * Create a new lazy collection base
     *
     * @param initialCacheSize The initial cache size
     */
    protected LazyCollection(
            int initialCacheSize
    ) {
        this.namespace = null;
        this.cache = new ArrayList<>(initialCacheSize);
    }

    // ----- Setters -----

    public void setNamespace(NamespaceValue namespace) {
        this.namespace = namespace;
    }

    // ----- Class methods -----

    /**
     * Initialize the cache to the given index
     * If the index is -1 this method should compute all the cache elements
     *
     * @param index The index to index the cache to
     */
    protected abstract void initCache(int index);

    // ----- Value methods -----

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.Indexable#get(int) */
    @Override
    public Object get(int index) throws InvalidIndexException {
        if(index >= 0) {
            // Initialize the cache to the given index
            this.initCache(index);

            // Try to get the cache element
            if(index < this.cache.size()) return this.cache.get(index);
        }

        // If the index is not a valid one, throw an exception
        throw new InvalidIndexException();
    }

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.Indexable#getContent() */
    @Override
    public Object[] getContent() {
        this.initCache(-1);
        return this.cache.toArray(new Object[0]);
    }

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.Iterable#size() */
    @Override
    public long size() {
        this.initCache(-1);
        return this.cache.size();
    }

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.Iterable#contains(Object) */
    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean contains(Object elem) {
        // Cast the element as an LKQL value
        LKQLValue lkqlElem = null;
        if(elem instanceof LKQLValue) lkqlElem = (LKQLValue) elem;

        // Iterate over all list elements to compare them
        Iterator iterator = this.iterator();
        while(iterator.hasNext()) {
            Object next = iterator.next();
            if((next instanceof LKQLValue lkqlNext) && lkqlElem != null) {
                if(lkqlElem.internalEquals(lkqlNext)) return true;
            } else {
                if(Objects.equals(elem, next)) return true;
            }
        }
        return false;
    }

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.Iterable#iterator() */
    @Override
    public Iterator iterator() {
        return new LazyCollectionIterator(this);
    }

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.Truthy#isTruthy() */
    @Override
    public boolean isTruthy() {
        this.initCache(0);
        return this.cache.size() > 0;
    }

    /** @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(LKQLValue) */
    @Override
    @CompilerDirectives.TruffleBoundary
    public boolean internalEquals(LKQLValue o) {
        if(o == this) return true;
        if(!(o instanceof LazyCollection other)) return false;
        if(this.size() != other.size()) return false;
        for(int i = 0 ; i < this.size() ; i++) {
            Object mineObject = this.get(i);
            Object hisObject = other.get(i);
            if((mineObject instanceof LKQLValue mine) && (hisObject instanceof LKQLValue his)) {
                if(!mine.internalEquals(his)) return false;
            } else {
                if(!Objects.equals(mineObject, hisObject)) return false;
            }
        }
        return true;
    }

    // ----- Override methods -----

    @Override
    @CompilerDirectives.TruffleBoundary
    public String toString() {
        // Create the string in a builder
        StringBuilder builder = new StringBuilder();
        builder.append('[');
        Iterator iterator = this.iterator();
        while(iterator.hasNext()) {
            Object value = iterator.next();
            if(value == null) {
                builder.append("null");
            } else if(value instanceof String s) {
                builder.append(StringUtils.toRepr(s));
            } else {
                builder.append(value);
            }
            if(iterator.hasNext()) {
                builder.append(", ");
            }
        }
        builder.append(']');

        // Return the tuple string representation
        return builder.toString();
    }

    // ----- Inner classes -----

    /**
     * This class represents an iterator for a lazy collection
     */
    public static class LazyCollectionIterator implements Iterator {

        // ----- Attributes -----

        /** The lazy collection to iterate on */
        private final LazyCollection lazyCollection;

        /** The pointer for the iterator */
        private int pointer;

        // ----- Constructors -----

        /**
         * Create the lazy collection iterator with the collection to iterate on
         *
         * @param lazyCollection The lazy collection
         */
        public LazyCollectionIterator(
                LazyCollection lazyCollection
        ) {
            this.lazyCollection = lazyCollection;
            this.pointer = 0;
        }

        // ----- Iteration methods -----

        /** @see com.adacore.lkql_jit.utils.util_classes.Iterator#hasNext() */
        @Override
        public boolean hasNext() {
            try {
                this.lazyCollection.get(this.pointer);
                return true;
            } catch (InvalidIndexException e) {
                return false;
            }
        }

        /** @see com.adacore.lkql_jit.utils.util_classes.Iterator#next() */
        @Override
        public Object next() {
            return this.lazyCollection.get(this.pointer++);
        }

        /** @see com.adacore.lkql_jit.utils.util_classes.Iterator#reset() */
        @Override
        public void reset() {
            this.pointer = 0;
        }

    }

}
