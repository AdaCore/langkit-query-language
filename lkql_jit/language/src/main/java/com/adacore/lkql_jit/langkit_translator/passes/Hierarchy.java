//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import com.adacore.libadalang.Libadalang;
import com.adacore.libadalang.Libadalang.AdaNode;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import java.util.*;

/**

    This class is a representation of a totally ordered set of elements S.
    It is used to store the class hierarchy of arbitrarily named classes.

    Each class X can have one super-class Y, which we note X < Y.
    Because this table is used to query `instanceof` operations, we store
    the relation `≤` rather than `<`.

    The relations are stored in a matrix of size |S|².
    Example:

    The hierarchy

    <pre>.
        A
       / \
      B   C
         / \
        D   E
    </pre>

    with the index [A=0, B=1, C=2, D=3, E=4]

    is stored in this table (an `x` represents a `≤` relation)

    <pre>.
      A B C D E
    A x        
    B x x      
    C x   x    
    D x   x x  
    E x   x   x
    </pre>

    The matrix cannot be stored in a triangle, it must be a full square !
    To demonstrate this, consider the index [B=0, C=1, D=2, E=3, A=4]
    putting A to the end, the table becomes:

    <pre>.
      B C D E A
    B x       x
    C   x     x
    D   x x   x
    E   x   x x
    A         x
    </pre>

    Because the table is stored as a flat array
    the X ≤ Y relation is found at `index(X) * size + index(Y)`
    of the array.

*/
public class Hierarchy {

    private int classCount;
    private boolean[] inheritanceMatrix;
    private HashMap<String, Integer> classNamesToIndex;

    /** Setter for all the fields of the class. */
    private void become(int size, boolean[] matrix, HashMap<String, Integer> index) {
        this.classCount = size;
        this.inheritanceMatrix = matrix;
        this.classNamesToIndex = index;
    }

    /**
     * Computes a hierarchy of currently available classes in the Lkt bindings.
     */
    @SuppressWarnings("unchecked")
    @TruffleBoundary
    public static Hierarchy initial() {
        // Collect all inital classes
        final Class<? extends AdaNode>[] initialClasses = Libadalang.NODE_DESCRIPTION_MAP.values()
            .stream()
            .map(e -> e.clazz)
            .toArray(Class[]::new);

        final int size = initialClasses.length;

        // Compute X ≤ Y matrix
        final boolean[] matrix = new boolean[size * size];

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                matrix[i * size + j] = initialClasses[j].isAssignableFrom(initialClasses[i]);
            }
        }

        // Assign an integer in 0..<N to each class
        final HashMap<String, Integer> index = new HashMap<>(size);
        for (int i = 0; i < size; i++) {
            index.put(initialClasses[i].getSimpleName(), i);
        }

        final var res = new Hierarchy();
        res.become(size, matrix, index);
        return res;
    }

    /**
     * The number of classes present in the hierarchy
     */
    public int size() {
        return classCount;
    }

    /**
     * Main query method for the hierarchy.
     * This is equivalent to `classY.isAssignableFrom(classX)`.
     * In ordered set notation this corresponds to X ≤ Y.
     *
     * @see java.lang.Class.isAssignableFrom(Class<?> cls)
     */
    @TruffleBoundary
    public boolean isInstance(String classX, String classY) {
        return inheritanceMatrix[classNamesToIndex.get(classX) * size() +
            classNamesToIndex.get(classY)];
    }

    /**
     * Add a new class to the hierarchy, with no superclass nor subclass.
     */
    @TruffleBoundary
    public void add(String classX) {
        addAll(Collections.singleton(classX));
    }

    /**
     * Add new classes to the hierarchy, with no superclasses nor subclasses.
     */
    @TruffleBoundary
    public void addAll(Collection<String> classes) {
        final int size = this.size() + classes.size();
        final boolean[] matrix = new boolean[size * size];

        // Copy original matrix
        for (int i = 0; i < this.size(); i++) {
            for (int j = 0; j < this.size(); j++) {
                matrix[i * size + j] = this.inheritanceMatrix[i * this.size() + j];
            }
        }

        // Forall I . I ≤ I
        for (int i = 0; i < size; i++) {
            matrix[i * size + i] = true;
        }

        final var index = new HashMap<>(this.classNamesToIndex);
        int i = this.size();
        for (var classI : classes) {
            index.put(classI, i++);
        }

        become(size, matrix, index);
    }

    /**
     * Add a new X ≤ Y relation to the hierarchy.
     */
    @TruffleBoundary
    public void addInstanceOfRelation(String classX, String classY) {
        final int x = classNamesToIndex.get(classX);
        final int y = classNamesToIndex.get(classY);

        inheritanceMatrix[x * size() + y] = true; // X ≤ Y

        // forall I , I ≤ X => I ≤ Y
        for (int i = 0; i < size(); i++) {
            inheritanceMatrix[i * size() + y] |= inheritanceMatrix[i * size() + x];
        }
        // forall I , Y ≤ I => X ≤ I
        for (int i = 0; i < size(); i++) {
            inheritanceMatrix[x * size() + i] |= inheritanceMatrix[y * size() + i];
        }
    }

    /**
     * Remove a class and all its subclasses from the hierarchy.
     */
    @TruffleBoundary
    public void remove(String classX) {
        removeAll(Collections.singleton(classX));
    }

    /**
     * Remove all classes and all their subclasses from the hierarchy.
     */
    @TruffleBoundary
    public void removeAll(Collection<String> classes) {
        // Compute all the subtypes of X to mask wich row/column to keep
        final boolean[] deleteMask = new boolean[size()];
        int deleteCount = 0;
        for (var classX : classes) {
            final int x = classNamesToIndex.get(classX);
            // forall I , I ≤ X => I in delete
            for (int i = 0; i < size(); i++) {
                if (inheritanceMatrix[i * size() + x] && !deleteMask[i]) {
                    deleteMask[i] = true;
                    deleteCount++;
                }
            }
        }

        // Copy relevant rows/columns to new matrix
        final int size = this.size() - deleteCount;
        final boolean[] matrix = new boolean[size * size];

        int iCursor = 0;
        for (int i = 0; i < this.size(); i++) {
            if (deleteMask[i]) continue;

            int jCursor = 0;
            for (int j = 0; j < this.size(); j++) {
                if (deleteMask[j]) continue;

                matrix[iCursor * size + jCursor] = this.inheritanceMatrix[i * this.size() + j];

                jCursor++;
            }

            iCursor++;
        }

        // Adjust indexes by shifting them down
        final var reverseIndex = new String[this.size()];
        for (var entry : this.classNamesToIndex.entrySet()) {
            reverseIndex[entry.getValue()] = entry.getKey();
        }

        final var index = new HashMap<String, Integer>(size);

        int cursor = 0;
        for (int i = 0; i < this.size(); i++) {
            if (deleteMask[i]) continue;
            index.put(reverseIndex[i], cursor);
            cursor++;
        }

        become(size, matrix, index);
    }
}
