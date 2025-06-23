//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime;

import com.adacore.lkql_jit.exception.utils.InvalidIndexException;

/**
 * Custom ArrayList like storage class. Meant to be used in our own data structures and control
 * flow. Useful so that Truffle's inliner can inline those operations in the compiled code
 * directly, making for pretty much zero overhead storage.
 */
public class ListStorage<T> {

    private Object[] storage;

    private int current_size = 0;
    private int capacity;

    public ListStorage(int startSize) {
        storage = new Object[startSize];
        capacity = startSize;
    }

    public void append(T item) {
        if (current_size >= capacity) {
            capacity *= 2;
            var new_storage = new Object[capacity];
            for (int i = 0; i < current_size; i++) {
                new_storage[i] = storage[i];
            }
            storage = new_storage;
        }
        storage[current_size] = item;
        current_size++;
    }

    public void pop() {
        current_size--;
        storage[current_size] = null;
    }

    public int size() {
        return current_size;
    }

    public T get(int idx) throws InvalidIndexException {
        if (idx < current_size) {
            return (T) storage[idx];
        }
        throw new InvalidIndexException();
    }

    public void set(int idx, T el) throws InvalidIndexException {
        if (idx < current_size) {
            storage[idx] = el;
        }
        throw new InvalidIndexException();
    }
}
