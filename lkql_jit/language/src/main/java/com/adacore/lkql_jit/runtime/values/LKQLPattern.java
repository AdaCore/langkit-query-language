//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.runtime.values;

import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import java.util.Objects;

/**
 * This class represents the pattern values in the LKQL language. Pattern values are compiled
 * regular expression.
 */
@ExportLibrary(InteropLibrary.class)
public final class LKQLPattern extends BasicLKQLValue {

    // ----- Attributes -----

    /** Members of the pattern LKQL value. */
    private static final LKQLList MEMBERS = new LKQLList(new String[] { "contains", "find" });

    /** The interop regex object from the TRegex language. */
    private final Object regexObject;

    /** The source string of the regex. */
    private final String regexString;

    /** Whether the regex is case-sensitive. */
    private final boolean caseSensitive;

    // ----- Constructors -----

    /** Create a new pattern value with the regex string. */
    private LKQLPattern(
        final Object regexObject,
        final String regexString,
        final boolean caseSensitive
    ) {
        this.regexObject = regexObject;
        this.regexString = regexString;
        this.caseSensitive = caseSensitive;
    }

    /**
     * Create a new LKQL pattern.
     *
     * @param regex Python flavored regular expression.
     * @param env Truffle environment to use to call the TRegex language.
     */
    @CompilerDirectives.TruffleBoundary
    public static LKQLPattern create(String regex, boolean caseSensitive, TruffleLanguage.Env env) {
        var regexString = "Flavor=Python/" + regex + (caseSensitive ? "/" : "/i");
        return new LKQLPattern(
            env
                .parseInternal(
                    Source.newBuilder("regex", regexString, "lkql_jit").internal(true).build()
                )
                .call(),
            regex,
            caseSensitive
        );
    }

    // ----- Instance methods -----

    /** Get whether the given string contains a substring that validate the regex. */
    public boolean contains(String string) {
        try {
            Object resultObject = InteropLibrary.getUncached()
                .invokeMember(this.regexObject, "exec", string, 0);
            return (boolean) InteropLibrary.getUncached().readMember(resultObject, "isMatch");
        } catch (Exception e) {
            throw new RuntimeException(
                StringUtils.concat("Pattern execution failed: ", this.regexString),
                e
            );
        }
    }

    /** Get the index of the first matched group in the given string, return -1 if there is none. */
    public int find(String string) {
        try {
            Object resultObject = InteropLibrary.getUncached()
                .invokeMember(this.regexObject, "exec", string, 0);
            return (int) InteropLibrary.getUncached().invokeMember(resultObject, "getStart", 0);
        } catch (Exception e) {
            throw new RuntimeException(
                StringUtils.concat("Pattern execution failed: ", this.regexString),
                e
            );
        }
    }

    // ----- Value methods -----

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
        return (ObjectUtils.equals(member, "contains") || ObjectUtils.equals(member, "find"));
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
        if (!(args[0] instanceof String arg)) {
            throw UnsupportedTypeException.create(args, "String is required as first argument");
        }

        // Call the valid method
        return switch (member) {
            case "contains" -> this.contains(arg);
            case "find" -> this.find(arg);
            default -> throw UnknownIdentifierException.create(member);
        };
    }

    // ----- Override methods -----

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LKQLPattern other)) return false;
        return (
            this.regexString.equals(other.regexString) && this.caseSensitive == other.caseSensitive
        );
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.regexString, this.caseSensitive);
    }
}
