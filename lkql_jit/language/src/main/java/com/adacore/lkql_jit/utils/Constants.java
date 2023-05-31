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

package com.adacore.lkql_jit.utils;


/**
 * This class contains all constant values of the LKQL JIT implementation.
 *
 * @author Hugo GUERRIER
 */
public class Constants {

    // ----- Enums -----

    /**
     * Enum representing the checker mode of a function
     */
    public enum CheckerMode {
        OFF,
        NODE,
        UNIT
    }

    /**
     * The kind of rendering to use when emitting diagnostic.
     */
    public enum DiagnosticOutputMode {
        /**
         * Emit a pretty diagnostic with source listing where the diagnostic location is highlighted
         */
        PRETTY,

        /**
         * Use a GNATCheck-compliant format: "{file}:{line}:{col} check: {message} [{check}]"
         */
        GNATCHECK
    }

    // ----- LKQL values -----

    /**
     * Identifier of LKQL in the GraalVM system.
     */
    public static final String LKQL_ID = "lkql";

    /**
     * LKQL files extension.
     */
    public static final String LKQL_EXTENSION = ".lkql";

    /**
     * MIME type for LKQL.
     */
    public static final String LKQL_MIME = "application/langkit-query-language";

    // ----- Special annotations -----

    /**
     * The memoization annotation string value.
     */
    public static final String ANNOTATION_MEMOIZED = "memoized";

    /**
     * The node checker annotation string value.
     */
    public static final String ANNOTATION_NODE_CHECK = "check";

    /**
     * The unit checker annotation string value.
     */
    public static final String ANNOTATION_UNIT_CHECK = "unit_check";

    // ----- Checker annotation helpers -----

    /**
     * The names of the parameters for a checker annotation
     */
    public static final String[] CHECKER_PARAMETER_NAMES = new String[]{
        "message",
        "help",
        "follow_generic_instantiations",
        "category",
        "subcategory",
        "remediation",
        "execution_cost",
        "parametric_exemption",
        "impact",
        "target"
    };

    /**
     * The default values for annotation parameters
     */
    public static final Object[] CHECKER_PARAMETER_DEFAULT_VALUES = new Object[]{
        null,
        null,
        false,
        "Misc",
        "Misc",
        "MEDIUM",
        0L,
        false,
        "",
        "amd64"
    };

    /**
     * The valid value for the remediation parameters
     */
    public static final String[] CHECKER_VALID_REMEDIATION = new String[]{
        "EASY",
        "MEDIUM",
        "MAJOR"
    };

    // ----- Unit values -----

    /**
     * Default name of a function.
     */
    public static final String FUNCTION_DEFAULT_NAME = "lambda";

    /**
     * Default documentation of a function.
     */
    public static final String FUNCTION_DEFAULT_DOC = "";

}
