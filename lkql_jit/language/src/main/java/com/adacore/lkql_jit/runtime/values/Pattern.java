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

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.source.Source;

/**
 * This class represents the pattern values in the LKQL language
 * Pattern values are compiled regular expression
 *
 * @author Hugo GUERRIER
 */
public final class Pattern implements LKQLValue {

    // ----- Attributes -----

    /**
     * The string of the regex
     */
    private final String regexString;

    /**
     * If the regex is case-sensitive
     */
    private final boolean caseSensitive;

    /**
     * The regex object
     */
    private final Object regexObject;

    // ----- Constructors -----

    /**
     * Create a new pattern value with the regex string
     *
     * @param creator       The creator of the pattern
     * @param regexString   The regex string
     * @param caseSensitive If the regex is case-sensitive
     */
    @CompilerDirectives.TruffleBoundary
    public Pattern(
        LKQLNode creator,
        String regexString,
        boolean caseSensitive
    ) {
        this.regexString = regexString;
        this.caseSensitive = caseSensitive;

        // Prepare the regex string
        String regexSource = "Flavor=PythonStr/" +
            regexString +
            (caseSensitive ? "/" : "/i");

        // Call the TRegex language
        try {
            this.regexObject = LKQLLanguage.getContext(creator).getEnv().parseInternal(
                Source.newBuilder("regex", regexSource, "lkql_jit")
                    .internal(true)
                    .build()
            ).call();
        } catch (AbstractTruffleException e) {
            throw LKQLRuntimeException.regexSyntaxError(regexString, creator);
        }
    }

    // ----- Getters -----

    public String getRegexString() {
        return regexString;
    }

    // ----- Class methods -----

    /**
     * Get if the given string contains a substring that validate the regex
     *
     * @param string The string to search in
     * @return True if the string validate the pattern, false else
     */
    public boolean contains(String string) {
        try {
            Object resultObject = InteropLibrary.getUncached().invokeMember(this.regexObject, "exec", string, 0);
            return (boolean) InteropLibrary.getUncached().readMember(resultObject, "isMatch");
        } catch (Exception e) {
            throw LKQLRuntimeException.fromMessage(StringUtils.concat("Pattern execution failed: ", this.regexString));
        }
    }

    /**
     * Get the index of the first matched group in the given string
     *
     * @param string The string to match
     * @return The index of the first matcher group or -1
     */
    public int find(String string) {
        try {
            Object resultObject = InteropLibrary.getUncached().invokeMember(this.regexObject, "exec", string, 0);
            return (int) InteropLibrary.getUncached().invokeMember(resultObject, "getStart", 0);
        } catch (Exception e) {
            throw LKQLRuntimeException.fromMessage(StringUtils.concat("Pattern execution failed: ", this.regexString));
        }
    }

    // ----- Value methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue#internalEquals(com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue)
     */
    @Override
    public boolean internalEquals(LKQLValue o) {
        if (o == this) return true;
        if (!(o instanceof Pattern other)) return false;
        return other.regexString.equals(this.regexString) && (this.caseSensitive && other.caseSensitive);
    }

    // ----- Override methods -----

    @Override
    public String toString() {
        return this.regexString;
    }

}
