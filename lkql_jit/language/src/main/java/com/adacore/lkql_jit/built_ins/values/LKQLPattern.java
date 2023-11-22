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

package com.adacore.lkql_jit.built_ins.values;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.utilities.TriState;

/**
 * This class represents the pattern values in the LKQL language. Pattern values are compiled
 * regular expression.
 */
@ExportLibrary(InteropLibrary.class)
public final class LKQLPattern extends BasicLKQLValue {

    // ----- Attributes -----

    /** Members of the pattern LKQL value. */
    private final LKQLList MEMBERS = new LKQLList(new String[] {"contains", "find"});

    /** The interop regex object from the TRegex language. */
    private final Object regexObject;

    /** The source string of the regex. */
    private final String regexString;

    /** Whether the regex is case-sensitive. */
    private final boolean caseSensitive;

    // ----- Constructors -----

    /**
     * Create a new pattern value with the regex string.
     *
     * @param creator The creator of the pattern.
     * @param regexString The regex string.
     * @param caseSensitive Whether the regex is case-sensitive.
     */
    @CompilerDirectives.TruffleBoundary
    public LKQLPattern(
            final LKQLNode creator, final String regexString, final boolean caseSensitive) {
        this.regexString = regexString;
        this.caseSensitive = caseSensitive;

        // Prepare the regex string
        String regexSource = "Flavor=PythonStr/" + regexString + (caseSensitive ? "/" : "/i");

        // Call the TRegex language
        try {
            this.regexObject =
                    LKQLLanguage.getContext(creator)
                            .getEnv()
                            .parseInternal(
                                    Source.newBuilder("regex", regexSource, "lkql_jit")
                                            .internal(true)
                                            .build())
                            .call();
        } catch (AbstractTruffleException e) {
            throw LKQLRuntimeException.regexSyntaxError(regexString, creator);
        }
    }

    // ----- Instance methods -----

    /** Get whether the given string contains a substring that validate the regex. */
    public boolean contains(String string) {
        try {
            Object resultObject =
                    InteropLibrary.getUncached().invokeMember(this.regexObject, "exec", string, 0);
            return (boolean) InteropLibrary.getUncached().readMember(resultObject, "isMatch");
        } catch (Exception e) {
            throw LKQLRuntimeException.fromMessage(
                    StringUtils.concat("Pattern execution failed: ", this.regexString));
        }
    }

    /** Get the index of the first matched group in the given string, return -1 if there is none. */
    public int find(String string) {
        try {
            Object resultObject =
                    InteropLibrary.getUncached().invokeMember(this.regexObject, "exec", string, 0);
            return (int) InteropLibrary.getUncached().invokeMember(resultObject, "getStart", 0);
        } catch (Exception e) {
            throw LKQLRuntimeException.fromMessage(
                    StringUtils.concat("Pattern execution failed: ", this.regexString));
        }
    }

    // ----- Value methods -----

    /** Exported message to compare two LKQL patterns. */
    @ExportMessage
    public static class IsIdenticalOrUndefined {
        /** Compare two LKQL patterns. */
        @Specialization
        protected static TriState onPattern(final LKQLPattern left, final LKQLPattern right) {
            return TriState.valueOf(
                    left.regexString.equals(right.regexString)
                            && left.caseSensitive == right.caseSensitive);
        }

        /** Do the comparison with another element. */
        @Fallback
        protected static TriState onOther(
                @SuppressWarnings("unused") final LKQLPattern receiver,
                @SuppressWarnings("unused") final Object other) {
            return TriState.UNDEFINED;
        }
    }

    /** Return the identity hash code for the given LKQL pattern. */
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public static int identityHashCode(final LKQLPattern receiver) {
        return System.identityHashCode(receiver);
    }

    /** Get the displayable string for the interop library. */
    @Override
    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    public String toDisplayString(@SuppressWarnings("unused") final boolean allowSideEffects) {
        return "pattern<\"" + this.regexString + "\">";
    }

    /** Tell the interop library that the value has members. */
    @ExportMessage
    public boolean hasMembers() {
        return true;
    }

    /** Get the existing members on the pattern. */
    @ExportMessage
    public Object getMembers(@SuppressWarnings("unused") final boolean includeInternal) {
        return MEMBERS;
    }

    /** Tell the interop library whether the given member is invokable. */
    @ExportMessage
    public boolean isMemberInvocable(String member) {
        return ObjectUtils.equals(member, "contains") || ObjectUtils.equals(member, "find");
    }

    /**
     * Call the given member on the pattern object.
     *
     * @throws ArityException If there is an error in the argument arity.
     * @throws UnsupportedTypeException If the given arguments hasn't the good type.
     * @throws UnknownIdentifierException If the provided member doesn't exist.
     */
    @ExportMessage
    public Object invokeMember(String member, Object... args)
            throws ArityException, UnsupportedTypeException, UnknownIdentifierException {
        // Verify the argument number
        if (args.length < 1) {
            throw ArityException.create(1, 1, args.length);
        }

        // Get the first arguments as a string
        if (!LKQLTypeSystemGen.isString(args[0])) {
            throw UnsupportedTypeException.create(args, "String is required as first argument");
        }
        final String arg = LKQLTypeSystemGen.asString(args[0]);

        // Call the valid method
        return switch (member) {
            case "contains" -> this.contains(arg);
            case "find" -> this.find(arg);
            default -> throw UnknownIdentifierException.create(member);
        };
    }
}
