//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Nullish;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.utilities.TriState;
import java.util.Collections;
import java.util.Map;

/** This class represents the null value in LKQL. */
@ExportLibrary(InteropLibrary.class)
public class LKQLNull implements LangkitSupport.NodeInterface, LKQLValue, Nullish {

    // ----- Attributes -----

    /** The sole instance of the LKQL null value. */
    public static final LKQLNull INSTANCE = new LKQLNull();

    /** The identity hash of the only instance of the LKQL null. */
    private static final int IDENTITY_HASH = System.identityHashCode(INSTANCE);

    // ----- NodeInterface methods -----

    @Override
    public LangkitSupport.Reflection.Node getDescription() {
        return null;
    }

    @Override
    public String[] getFieldNames() {
        return new String[0];
    }

    @Override
    public Map<String, ? extends LangkitSupport.Reflection.Field> getFieldDescriptions() {
        return Collections.emptyMap();
    }

    @Override
    public LangkitSupport.Reflection.Field getFieldDescription(final String name) {
        return null;
    }

    @Override
    public boolean isListNode() {
        return false;
    }

    @Override
    public boolean isTokenNode() {
        return false;
    }

    @Override
    public LangkitSupport.TokenInterface tokenStart() {
        return null;
    }

    @Override
    public LangkitSupport.TokenInterface tokenEnd() {
        return null;
    }

    @Override
    public boolean isNone() {
        return true;
    }

    @Override
    public LangkitSupport.AnalysisUnit getUnit() {
        return null;
    }

    @Override
    public LangkitSupport.NodeInterface[] children() {
        return new LKQLNull[0];
    }

    @Override
    public int getChildrenCount() {
        return 0;
    }

    @Override
    public LangkitSupport.NodeInterface getChild(final int n) {
        return new LKQLNull();
    }

    @Override
    public String getText() {
        return null;
    }

    @Override
    public String getImage() {
        return this.getText();
    }

    @Override
    public LangkitSupport.SourceLocationRange getSourceLocationRange() {
        return new LangkitSupport.SourceLocationRange(
            new LangkitSupport.SourceLocation(-1, (short) 1),
            new LangkitSupport.SourceLocation(-1, (short) 1)
        );
    }

    @Override
    public LangkitSupport.RewritingNodeInterface getRewritingNode() {
        return null;
    }

    @Override
    public String dumpTree() {
        return this.getImage();
    }

    @Override
    public void dumpTree(final StringBuilder builder) {
        builder.append(this.dumpTree());
    }

    @Override
    public LangkitSupport.NodeInterface parent() {
        return new LKQLNull();
    }

    // ----- Value methods -----

    /** Tell the interop API that the value has an associated language. */
    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    /** Give the LKQL language class to the interop library. */
    @ExportMessage
    Class<? extends TruffleLanguage<?>> getLanguage() {
        return LKQLLanguage.class;
    }

    /** Tell the interop API if the given other value is null. */
    @ExportMessage
    static TriState isIdenticalOrUndefined(
        @SuppressWarnings("unused") final LKQLNull receiver,
        final Object other
    ) {
        return TriState.valueOf(receiver == other);
    }

    /**
     * Return the identity hash code for the given receiver (always the same because null value is
     * singleton).
     */
    @ExportMessage
    static int identityHashCode(@SuppressWarnings("unused") final LKQLNull receiver) {
        return IDENTITY_HASH;
    }

    /** Get the displayable string for the interop library. */
    @ExportMessage
    Object toDisplayString(@SuppressWarnings("unused") boolean allowSideEffect) {
        return "null";
    }

    /** Tell the interop API that the value is nullish. */
    @ExportMessage
    boolean isNull() {
        return true;
    }

    /** Tell the interop API that the value is a boolean like value. */
    @ExportMessage
    boolean isBoolean() {
        return true;
    }

    /** Get the boolean like value from null, which is always false. */
    @ExportMessage
    boolean asBoolean() {
        return false;
    }

    @Override
    public String toString() {
        return "null";
    }

    @Override
    public boolean equals(Object o) {
        return o == this;
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
