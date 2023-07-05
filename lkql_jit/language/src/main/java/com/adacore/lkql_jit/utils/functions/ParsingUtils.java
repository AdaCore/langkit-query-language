/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

package com.adacore.lkql_jit.utils.functions;


import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLTypeSystem;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.langkit_translator.LangkitTranslator;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.runtime.values.NamespaceValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLConfigFileResult;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.source.Source;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class contains functions that help parsing and extracting information for LKQL configuration.
 *
 * @author Hugo GUERRIER
 */
public class ParsingUtils {

    // ----- Rules arguments from command line -----

    /**
     * Parse the command line rule arguments.
     *
     * @param argsSource The source of rule arguments to parse.
     * @return The map containing the rules arguments.
     */
    public static Map<String, Map<String, Object>> parseRulesArgs(
        final String[] argsSource
    ) {
        // Prepare the result
        Map<String, Map<String, Object>> res = new HashMap<>();

        for (String ruleArgSource : argsSource) {
            // Verify that the rule is not empty
            if (ruleArgSource.isEmpty() || ruleArgSource.isBlank()) continue;

            // Split the get the names and the value
            final String[] valueSplit = ruleArgSource.split("=");
            final String[] nameSplit = valueSplit[0].split("\\.");

            // Verify the rule argument syntax
            if (valueSplit.length != 2 || nameSplit.length != 2) {
                throw LKQLRuntimeException.fromMessage("Rule argument syntax error : '" + ruleArgSource + "'");
            }

            // Get the information from the rule argument source
            final String ruleName = nameSplit[0].toLowerCase().trim();
            final String argName = nameSplit[1].toLowerCase().trim();
            final String valueSource = valueSplit[1].trim();

            // Parse the value source with Liblkqllang
            final Object argValue;
            try (Liblkqllang.AnalysisContext analysisContext = Liblkqllang.AnalysisContext.create()) {
                // Parse the argument value source with Liblkqllang
                final Liblkqllang.AnalysisUnit unit = analysisContext.getUnitFromBuffer(
                    valueSource,
                    "rule_argument",
                    null,
                    Liblkqllang.GrammarRule.EXPR_RULE
                );
                final Liblkqllang.LkqlNode root = unit.getRoot();

                // Validate the argument value node
                if (!isValidRuleArgument(root)) {
                    throw LKQLRuntimeException.fromMessage("The rule argument value must be an LKQL literal : " + valueSource);
                }

                // Execute the value source with LKQL implementation
                final Source source = Source.newBuilder(Constants.LKQL_ID, valueSource, "rule_argument").build();
                final LKQLNode node = LangkitTranslator.translate(root, source);
                argValue = node.executeGeneric(null);
            }

            // Add the argument in the result
            Map<String, Object> ruleArgs = res.getOrDefault(ruleName, new HashMap<>());
            ruleArgs.put(argName, argValue);
            res.put(ruleName, ruleArgs);
        }

        // Return the result
        return res;
    }

    /**
     * Get if the given LKQL node is a valid argument node.
     *
     * @param argumentNode The argument to validate.
     * @return True if the node is a valid argument node, false else.
     */
    private static boolean isValidRuleArgument(Liblkqllang.LkqlNode argumentNode) {
        // If the node is just a literal it's value
        if (argumentNode instanceof Liblkqllang.Literal) return true;

            // Else if it's a tuple literal we must verify the expressions inside it
        else if (argumentNode instanceof Liblkqllang.Tuple tupleLiteral) {
            Liblkqllang.ExprList exprList = tupleLiteral.fExprs();
            int childrenCount = exprList.getChildrenCount();
            for (int i = 0; i < childrenCount; i++) {
                if (!isValidRuleArgument(exprList.getChild(i))) return false;
            }
            return true;
        }

        // Else if it's a list literal we must verify the expressions of the list
        else if (argumentNode instanceof Liblkqllang.ListLiteral listLiteral) {
            Liblkqllang.ExprList exprList = listLiteral.fExprs();
            int childrenCount = exprList.getChildrenCount();
            for (int i = 0; i < childrenCount; i++) {
                if (!isValidRuleArgument(exprList.getChild(i))) return false;
            }
            return true;
        }

        // Else if it's an object literal we must verify all association values
        else if (argumentNode instanceof Liblkqllang.ObjectLiteral objectLiteral) {
            Liblkqllang.ObjectAssocList assocList = objectLiteral.fAssocs();
            int childrenCount = assocList.getChildrenCount();
            for (int i = 0; i < childrenCount; i++) {
                Liblkqllang.ObjectAssoc assoc = (Liblkqllang.ObjectAssoc) assocList.getChild(i);
                if (!isValidRuleArgument(assoc.fExpr())) return false;
            }
            return true;
        }

        // By default return false
        return false;
    }

    // ----- LKQL rule configuration file -----

    /**
     * Parse the given LKQL file and extract the rules and configuration from it.
     *
     * @param context  The LKQL context to parse in.
     * @param LKQLFile The LKQL file to parse.
     * @return A triplet containing rules, aliases and arguments.
     */
    public static LKQLConfigFileResult parseLKQLConfigFile(
        final LKQLContext context,
        final String LKQLFile
    ) {
        try {
            // Prepare the values to return
            final List<String> allRules = new ArrayList<>();
            final List<String> adaRules = new ArrayList<>();
            final List<String> sparkRules = new ArrayList<>();
            final Map<String, String> aliases = new HashMap<>();
            final Map<String, Map<String, Object>> args = new HashMap<>();

            // Create a new source for the LKQL file and parse it
            final Source source = Source
                .newBuilder(Constants.LKQL_ID, context.getEnv().getPublicTruffleFile(LKQLFile))
                .build();
            final CallTarget callTarget = context.getEnv().parseInternal(source);

            // Get the namespace of the LKQL config file to extract rule configuration
            final NamespaceValue namespace = (NamespaceValue) context.getEnv().asHostObject(callTarget.call());

            // Get the rules
            final Object allRulesSource = namespace.get("rules");
            if (LKQLTypeSystemGen.isObjectValue(allRulesSource)) {
                final ObjectValue allRulesObject = LKQLTypeSystemGen.asObjectValue(allRulesSource);
                processRulesObject(allRulesObject, allRules, aliases, args);
            } else {
                throw LKQLRuntimeException.fromMessage("LKQL config file must define a 'rules' top level object value");
            }

            // Get the Ada rules (not mandatory)
            final Object adaRulesSource = namespace.get("ada_rules");
            if (LKQLTypeSystemGen.isObjectValue(adaRulesSource)) {
                final ObjectValue adaRulesObject = LKQLTypeSystemGen.asObjectValue(adaRulesSource);
                processRulesObject(adaRulesObject, adaRules, aliases, args);
            }

            // Get the SPARK rules (not mandatory)
            final Object sparkRulesSource = namespace.get("spark_rules");
            if (LKQLTypeSystemGen.isObjectValue(sparkRulesSource)) {
                final ObjectValue sparkRulesObject = LKQLTypeSystemGen.asObjectValue(sparkRulesSource);
                processRulesObject(sparkRulesObject, sparkRules, aliases, args);
            }

            // Return the result
            return new LKQLConfigFileResult(
                allRules,
                adaRules,
                sparkRules,
                aliases,
                args
            );
        } catch (IOException e) {
            throw LKQLRuntimeException.fromMessage(e.getMessage());
        }
    }

    /**
     * Process a rules object.
     *
     * @param rulesObject The rules object.
     * @param rules       The list containing all parsed rules.
     * @param aliases     The map containing all parsed aliases.
     * @param args        The map containing all parsed arguments.
     */
    private static void processRulesObject(
        final ObjectValue rulesObject,
        final List<String> rules,
        final Map<String, String> aliases,
        final Map<String, Map<String, Object>> args
    ) {
        // For each rule in the rules object explore its arguments
        for (String ruleName : rulesObject.getContent().keySet()) {
            // Get the rule arguments and check that it's an indexable value
            final Object ruleArgs = rulesObject.get(ruleName);
            if (!LKQLTypeSystemGen.isIndexable(ruleArgs)) {
                throw LKQLRuntimeException.fromMessage("Rule arguments must be an indexable value");
            }

            // For each rule argument check its type and parse it
            final Object[] ruleArgsContent = LKQLTypeSystemGen.asIndexable(ruleArgs).getContent();
            if (ruleArgsContent.length == 0) {
                rules.add(ruleName);
            } else {
                for (Object ruleArg : ruleArgsContent) {
                    if(!LKQLTypeSystemGen.isObjectValue(ruleArg)) {
                        throw LKQLRuntimeException.fromMessage("Rule argument must be an object value");
                    }
                    processArgObject(
                        ruleName,
                        LKQLTypeSystemGen.asObjectValue(ruleArg),
                        rules,
                        aliases,
                        args
                    );
                }
            }
        }
    }

    /**
     * Process an argument object.
     *
     * @param ruleName  The name of the rule.
     * @param argObject The argument object.
     * @param rules     The list containing all parsed rules.
     * @param aliases   The map containing all parsed aliases.
     * @param args      The map containing all parsed arguments.
     */
    private static void processArgObject(
        final String ruleName,
        final ObjectValue argObject,
        final List<String> rules,
        final Map<String, String> aliases,
        final Map<String, Map<String, Object>> args
    ) {
        // If the arg has an alias get the name of it and add it in the alias list
        final String realName;
        final Object aliasName = argObject.get(Constants.ALIAS_NAME_SYMBOL);
        if (aliasName != null) {
            if (!LKQLTypeSystemGen.isString(aliasName)) {
                throw LKQLRuntimeException.fromMessage("Rule alias name must be a string");
            }
            realName = LKQLTypeSystemGen.asString(aliasName);
            aliases.put(realName, ruleName);
        } else {
            realName = ruleName;
        }

        // Add the rule to the list
        rules.add(realName);

        // Get all other arguments
        for (String argName : argObject.getContent().keySet()) {
            // If the argument name is "alias_symbol" continue the exploration
            if (argName.equals(Constants.ALIAS_NAME_SYMBOL)) {
                continue;
            }

            // Put the argument in the map
            Object argValue = argObject.get(argName);
            Map<String, Object> ruleArgs = args.getOrDefault(realName, new HashMap<>());
            ruleArgs.put(argName, argValue);
            args.put(realName, ruleArgs);
        }
    }

}
