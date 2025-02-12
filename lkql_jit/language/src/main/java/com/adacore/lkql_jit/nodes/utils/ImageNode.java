//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.utils;

import com.adacore.lkql_jit.utils.Constants;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.strings.TruffleString;
import java.math.BigInteger;

/** Utility class used to get the image of any value in the LKQL engine. */
@GenerateUncached
public abstract class ImageNode extends Node {

    // ----- Constants -----

    private static final TruffleString FALSE = TruffleString.fromJavaStringUncached(
        "false",
        Constants.STRING_ENCODING
    );
    private static final TruffleString TRUE = TruffleString.fromJavaStringUncached(
        "true",
        Constants.STRING_ENCODING
    );

    // ----- Helpers ------

    @CompilerDirectives.TruffleBoundary
    private static String stringRepr(String source) {
        return "\"" + source.replace("\"", "\\\"").replace("\n", "\\x0a") + "\"";
    }

    @CompilerDirectives.TruffleBoundary
    private static String bigIntString(BigInteger bigInteger) {
        return bigInteger.toString();
    }

    // ----- Execution methods -----

    public abstract TruffleString execute(Object obj);

    @Specialization
    protected TruffleString onBool(boolean b) {
        return b ? TRUE : FALSE;
    }

    @Specialization
    protected TruffleString onLong(long l, @Cached TruffleString.FromLongNode fromLongNode) {
        return fromLongNode.execute(l, Constants.STRING_ENCODING, false);
    }

    @Specialization
    protected TruffleString onBigInteger(
        BigInteger bigInteger,
        @Cached TruffleString.FromJavaStringNode fromJavaStringNode
    ) {
        return fromJavaStringNode.execute(bigIntString(bigInteger), Constants.STRING_ENCODING);
    }

    @Specialization
    protected TruffleString onString(
        TruffleString str,
        @Cached TruffleString.ToJavaStringNode toJavaStringNode,
        @Cached TruffleString.FromJavaStringNode fromJavaStringNode
    ) {
        return fromJavaStringNode.execute(
            stringRepr(toJavaStringNode.execute(str)),
            Constants.STRING_ENCODING
        );
    }

    @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
    protected TruffleString onOthers(
        Object obj,
        @CachedLibrary(value = "obj") InteropLibrary objLibrary
    ) {
        return (TruffleString) objLibrary.toDisplayString(obj);
    }
}
