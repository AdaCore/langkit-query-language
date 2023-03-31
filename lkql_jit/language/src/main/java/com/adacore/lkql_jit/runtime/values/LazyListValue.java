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

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.dispatchers.ListComprehensionDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.ListComprehensionDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.root_nodes.ListComprehensionRootNode;
import com.adacore.lkql_jit.runtime.values.interfaces.LazyCollection;
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.CompilerDirectives;


/**
 * This class represents a lazy list in the LKQL language, this is the result of a list comprehension
 *
 * @author Hugo GUERRIER
 */
public final class LazyListValue extends LazyCollection {

    // ----- Attributes -----

    /**
     * The list of arguments for the root node
     */
    private final Object[][] argsList;

    /**
     * The pointer to the next lazy value to evaluate
     */
    private int lazyPointer;

    /**
     * The closure for the execution
     */
    private final Closure closure;

    /**
     * The root node for the list comprehension
     */
    private final ListComprehensionRootNode rootNode;

    /**
     * The dispatcher for the list comprehension root node
     */
    private final ListComprehensionDispatcher dispatcher;

    // ----- Constructors -----

    /**
     * Create a new lazy list value
     *
     * @param rootNode The root node for the execution
     * @param argsList The argument list
     */
    @CompilerDirectives.TruffleBoundary
    public LazyListValue(
        Closure closure,
        ListComprehensionRootNode rootNode,
        Object[][] argsList
    ) {
        super(argsList.length);
        this.closure = closure;
        this.rootNode = rootNode;
        this.argsList = argsList;
        this.dispatcher = ListComprehensionDispatcherNodeGen.create();
        LKQLContext context = LKQLLanguage.getContext(this.rootNode.getResult());
        if (context != null && context.getGlobalValues().getNamespaceStack().size() > 0) {
            this.namespace = context.getGlobalValues().getNamespaceStack().peek();
        }
    }

    // ----- Class methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LazyCollection#initCache(int)
     */
    @Override
    protected void initCache(int index) {
        LKQLContext context = LKQLLanguage.getContext(this.rootNode.getResult());
        boolean pushed = false;
        if (this.namespace != null && context.getGlobalValues().getNamespaceStack().peek() != this.namespace) {
            context.getGlobalValues().pushNamespace(this.namespace);
            pushed = true;
        }
        Closure saveClosure = this.rootNode.getClosure();
        try {
            this.rootNode.setClosure(this.closure);
            while (this.lazyPointer < this.argsList.length && (this.cache.size() - 1 < index || index == -1)) {
                Object value = this.dispatcher.executeDispatch(this.rootNode, this.argsList[this.lazyPointer++]);
                if (value != null) {
                    this.cache.add(value);
                }
            }
        } finally {
            if (pushed) {
                context.getGlobalValues().popNamespace();
            }
            this.rootNode.setClosure(saveClosure);
        }
    }

}
