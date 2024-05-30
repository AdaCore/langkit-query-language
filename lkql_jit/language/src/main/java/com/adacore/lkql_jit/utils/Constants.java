//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils;

/**
 * This class contains all constant values of the LKQL JIT implementation.
 *
 * @author Hugo GUERRIER
 */
public class Constants {

    // ----- JIT configuration ----

    /**
     * Number of internally dispatched specialized Truffle library. For more information see
     * https://www.graalvm.org/truffle/javadoc/com/oracle/truffle/api/library/CachedLibrary.html#limit--
     */
    public static final String DISPATCHED_LIB_LIMIT = "4";

    /**
     * Number of specialization instantiations. For more information see
     * https://www.graalvm.org/truffle/javadoc/com/oracle/truffle/api/dsl/Specialization.html#limit--
     */
    public static final String SPECIALIZED_LIB_LIMIT = "3";

    // ----- LKQL values -----

    /** Identifier of LKQL in the GraalVM system. */
    public static final String LKQL_ID = "lkql";

    /** LKQL files extension. */
    public static final String LKQL_EXTENSION = ".lkql";

    /** MIME type for LKQL. */
    public static final String LKQL_MIME = "application/langkit-query-language";

    /** Environment variable which contains paths to look LKQL scripts in. */
    public static final String LKQL_PATH = "LKQL_PATH";

    /** Environment variable which contains the paths to look LKQL rules in. */
    public static final String LKQL_RULES_PATH = "LKQL_RULES_PATH";

    // ----- Function default values -----

    /** Default name of a function. */
    public static final String FUNCTION_DEFAULT_NAME = "lambda";

    /** Default documentation of a function. */
    public static final String FUNCTION_DEFAULT_DOC = "";

    // ----- Built-in symbols -----

    /** Symbol which contains the "this" value in selectors. */
    public static final String THIS_SYMBOL = "this";

    /** Symbol which contains the depth in selectors. */
    public static final String DEPTH_SYMBOL = "depth";

    /** Symbol which contains the maximal depth value in selector calls. */
    public static final String MAX_DEPTH_SYMBOL = "max_depth";

    /** Symbol which contains the minimal depth value in selector calls. */
    public static final String MIN_DEPTH_SYMBOL = "min_depth";

    /** Special object key for LKQL config file to define rule alias. */
    public static final String ALIAS_NAME_SYMBOL = "instance_name";

    /** Special object key for LKQL config file to define the rule instance name. IOW rule alias. */
    public static final String INSTANCE_NAME_SYMBOL = "instance_name";

    // ----- Special annotations -----

    /** The memoization annotation string value. */
    public static final String ANNOTATION_MEMOIZED = "memoized";

    /** The node checker annotation string value. */
    public static final String ANNOTATION_NODE_CHECK = "check";

    /** The unit checker annotation string value. */
    public static final String ANNOTATION_UNIT_CHECK = "unit_check";

    // ----- Checker annotation helpers -----

    /** The names of the parameters for a checker annotation */
    public static final String[] CHECKER_PARAMETER_NAMES =
            new String[] {
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

    /** The default values for annotation parameters. */
    public static final Object[] CHECKER_PARAMETER_DEFAULT_VALUES =
            new Object[] {null, null, false, "Misc", "Misc", "MEDIUM", 0L, false, "", "amd64"};
}
