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

package com.adacore.lkql_jit.utils.util_classes;

import java.util.ArrayList;


/**
 * This class is a stack based on an array list
 *
 * @author Hugo GUERRIER
 */
public final class Stack<T> {

    // ----- Attributes -----

    /** The cache to handle the stack head */
    private T head;

    /** The content of the stack */
    private final ArrayList<T> content;

    // ----- Constructors -----

    /**
     * Create a new empty stack
     */
    public Stack() {
        this.head = null;
        this.content = new ArrayList<>();
    }

    // ----- Class methods -----

    /**
     * Add an element on the top of the stack
     *
     * @param elem The element to add
     */
    public void push(T elem) {
        this.content.add(elem);
        this.head = elem;
    }

    /**
     * Get the stack head without removing it
     *
     * @return The stack head
     */
    public T peek() {
        return this.head;
    }

    /**
     * Get the stack head and remove it from the stack
     *
     * @return The stack head or null if the stack is empty
     */
    public T pop() {
        if(this.content.size() > 0) {
            this.content.remove(this.content.size() - 1);
            if(this.content.size() > 0) {
                this.head = this.content.get(this.content.size() - 1);
            } else {
                this.head = null;
            }
        }
        return this.head;
    }

    /**
     * Get the size of the stack
     *
     * @return The size in an int
     */
    public int size() {
        return this.content.size();
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.content.toString();
    }

}
