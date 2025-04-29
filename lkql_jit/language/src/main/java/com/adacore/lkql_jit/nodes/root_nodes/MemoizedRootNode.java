//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.root_nodes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import java.util.HashMap;

/**
 * This root node is the base of all root node which can be memoized in LKQL.
 *
 * @author Hugo GUERRIER
 */
public abstract class MemoizedRootNode<K, V> extends BaseRootNode {

    // ----- Attributes -----

    /**
     * Cache to store the root node execution results.
     *
     * IMPORTANT: We don't use the {@link org.graalvm.collections.EconomicMap} class to represent
     * the cache because there is an implementation error causing invalid cache hits.
     */
    protected final HashMap<K, V> memoizationCache;

    // ----- Constructors -----

    /**
     * Create a new memoized root node.
     *
     * @param language The language instance which owns the root node.
     * @param frameDescriptor The descriptor of the frame for the root node execution.
     */
    protected MemoizedRootNode(
        final TruffleLanguage<?> language,
        final FrameDescriptor frameDescriptor
    ) {
        super(language, frameDescriptor);
        this.memoizationCache = new HashMap<>();
    }

    // ----- Instance methods -----

    /**
     * Get if the given key is a memoized call.
     *
     * @param key The key of the call.
     * @return True if the call has been done and the result is in the cache, false else.
     */
    @CompilerDirectives.TruffleBoundary
    protected boolean isMemoized(final K key) {
        return this.memoizationCache.containsKey(key);
    }

    /**
     * Get the result of the root node call for the given key.
     *
     * @param key The key of the call.
     * @return The result of the call with the key, null if it doesn't exist.
     */
    @CompilerDirectives.TruffleBoundary
    protected V getMemoized(final K key) {
        return this.memoizationCache.get(key);
    }

    /**
     * Associate the given call key with the given call value in the memoization cache.
     *
     * @param key The key to store in the cache.
     * @param value The value of the call with the key.
     */
    @CompilerDirectives.TruffleBoundary
    protected void putMemoized(final K key, final V value) {
        this.memoizationCache.put(key, value);
    }
}
