//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime;

/**
 * This class represents a storing cell in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Cell {

    // ----- Attributes -----

    /** The reference stored in the cell. */
    private Object ref;

    // ----- Constructors -----

    /** Create a new empty cell. */
    public Cell() {
        this.ref = null;
    }

    /**
     * Create a new cell with its reference.
     *
     * @param obj The object to store in the cell.
     */
    public Cell(Object obj) {
        this.ref = obj;
    }

    // ----- Getters -----

    public boolean isNull() {
        return this.ref == null;
    }

    public Object getRef() {
        return this.ref;
    }

    // ----- Setters -----

    public void setRef(Object ref) {
        this.ref = ref;
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.isNull() ? "null" : this.ref.toString();
    }
}
