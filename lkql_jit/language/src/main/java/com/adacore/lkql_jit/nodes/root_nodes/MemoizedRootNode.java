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

package com.adacore.lkql_jit.nodes.root_nodes;


import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import org.graalvm.collections.EconomicMap;

/**
 * This root node is the base of all root node which can be memoized in LKQL.
 *
 * @author Hugo GUERRIER
 */
public abstract class MemoizedRootNode<K, V> extends BaseRootNode {

    // ----- Attributes -----

    /**
     * Cache to store the root node execution results.
     */
    protected final EconomicMap<K, V> memoizationCache;

    // ----- Constructors -----

    /**
     * Create a new memoized root node.
     *
     * @param language        The language instance which owns the root node.
     * @param frameDescriptor The descriptor of the frame for the root node execution.
     */
    protected MemoizedRootNode(
        final TruffleLanguage<?> language,
        final FrameDescriptor frameDescriptor
    ) {
        super(language, frameDescriptor);
        this.memoizationCache = EconomicMap.create();
    }

    // ----- Instance methods -----

    /**
     * Get if the given key is a memoized call.
     *
     * @param key The key of the call.
     * @return True if the call has been done and the result is in the cache, false else.
     */
    @CompilerDirectives.TruffleBoundary
    protected boolean isMemoized(
        final K key
    ) {
        return this.memoizationCache.containsKey(key);
    }

    /**
     * Get the result of the root node call for the given key.
     *
     * @param key The key of the call.
     * @return The result of the call with the key, null if it doesn't exist.
     */
    @CompilerDirectives.TruffleBoundary
    protected V getMemoized(
        final K key
    ) {
        return this.memoizationCache.get(key, null);
    }

    /**
     * Associate the given call key with the given call value in the memoization cache.
     *
     * @param key   The key to store in the cache.
     * @param value The value of the call with the key.
     */
    @CompilerDirectives.TruffleBoundary
    protected void putMemoized(
        final K key,
        final V value
    ) {
        this.memoizationCache.put(key, value);
    }

}
