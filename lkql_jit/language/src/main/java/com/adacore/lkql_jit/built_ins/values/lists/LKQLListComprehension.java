/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins.values.lists;

import com.adacore.lkql_jit.nodes.dispatchers.ListComprehensionDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.ListComprehensionDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.root_nodes.ListComprehensionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** This class represents a list comprehension value in the LKQL language. */
@ExportLibrary(InteropLibrary.class)
public final class LKQLListComprehension extends LKQLLazyList {

    // ----- Attributes -----

    /** Root node of the list comprehension. */
    private final ListComprehensionRootNode rootNode;

    /** Dispatcher for the list comprehension root node. */
    private final ListComprehensionDispatcher dispatcher;

    /** List of arguments to pass to the list comprehension root node. */
    private final Object[][] argumentsList;

    /** Pre-allocated argument array which already contains the list comprehension closure. */
    private final Object[] arguments;

    /** Pointer to the current arguments to pass to the root node. */
    private int pointer;

    // ----- Constructors -----

    /**
     * Create a new LKQL list comprehension value with everything needed for its execution.
     *
     * @param rootNode The root node which contains the list comprehension logics.
     * @param closure The closure for the execution.
     * @param argumentsList The sequence of arguments to pass to the list comprehension root node.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLListComprehension(
            final ListComprehensionRootNode rootNode,
            final Closure closure,
            final Object[][] argumentsList) {
        this.rootNode = rootNode;
        this.dispatcher = ListComprehensionDispatcherNodeGen.create();
        this.argumentsList = argumentsList;
        this.arguments =
                this.argumentsList.length > 0
                        ? new Object[this.argumentsList[0].length + 1]
                        : new Object[1];
        this.arguments[0] = closure.getContent();
        this.pointer = 0;
    }

    // ----- Lazy list required methods -----

    @Override
    public void initCache(long n) {
        while (this.pointer < this.argumentsList.length && (this.cache.size() - 1 < n || n < 0)) {
            final Object[] currentArguments = this.argumentsList[this.pointer++];
            System.arraycopy(currentArguments, 0, this.arguments, 1, currentArguments.length);
            Object value = this.dispatcher.executeDispatch(this.rootNode, this.arguments);
            if (value != null) {
                this.cache.add(value);
            }
        }
    }

    // ----- Value methods -----

    /** Return the identity hash code for the given LKQL list comprehension. */
    @CompilerDirectives.TruffleBoundary
    @ExportMessage
    public static int identityHashCode(LKQLListComprehension receiver) {
        return System.identityHashCode(receiver);
    }
}
