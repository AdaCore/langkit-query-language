//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.utils.functions;

import com.adacore.liblkqllang.Liblkqllang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.values.LKQLNamespace;
import com.adacore.lkql_jit.built_ins.values.LKQLObject;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.LKQLConfigFileResult;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class contains functions that help parsing and extracting information for LKQL
 * configuration.
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
    public static Map<String, Map<String, Object>> parseRulesArgs(final String[] argsSource) {
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
                throw LKQLRuntimeException.fromMessage(
                        "Rule argument syntax error : '" + ruleArgSource + "'");
            }

            // Get the information from the rule argument source
            final String ruleName = nameSplit[0].toLowerCase().trim();
            final String argName = nameSplit[1].toLowerCase().trim();
            final String valueSource = valueSplit[1].trim();

            // Parse the value source with Liblkqllang
            try (Liblkqllang.AnalysisContext analysisContext =
                    Liblkqllang.AnalysisContext.create()) {
                // Parse the argument value source with Liblkqllang
                final Liblkqllang.AnalysisUnit unit =
                        analysisContext.getUnitFromBuffer(
                                valueSource,
                                "rule_argument",
                                null,
                                Liblkqllang.GrammarRule.EXPR_RULE);
                final Liblkqllang.LkqlNode root = unit.getRoot();
                final Source source =
                        Source.newBuilder(Constants.LKQL_ID, valueSource, "rule_argument").build();
                final LKQLNode node = LKQLContext.translate(root, source);

                try {
                    // Add the argument in the result
                    Map<String, Object> ruleArgs = res.getOrDefault(ruleName, new HashMap<>());
                    ruleArgs.put(argName, node.executeGeneric(null));
                    res.put(ruleName, ruleArgs);
                } catch (Exception e) {
                    throw LKQLRuntimeException.fromMessage(
                            "The rule argument value generated an interpreter error: "
                                    + valueSource);
                }
            }
        }

        // Return the result
        return res;
    }

    // ----- LKQL rule configuration file -----

    /**
     * Parse the given LKQL file and extract the rules and configuration from it.
     *
     * @param context The LKQL context to parse in.
     * @param LKQLFile The LKQL file to parse.
     * @return A triplet containing rules, aliases and arguments.
     */
    public static LKQLConfigFileResult parseLKQLConfigFile(
            final LKQLContext context, final String LKQLFile) {
        try {
            // Prepare the values to return
            final List<String> allRules = new ArrayList<>();
            final List<String> adaRules = new ArrayList<>();
            final List<String> sparkRules = new ArrayList<>();
            final Map<String, String> aliases = new HashMap<>();
            final Map<String, Map<String, Object>> args = new HashMap<>();

            // Create a new source for the LKQL file and parse it
            final Source source =
                    Source.newBuilder(
                                    Constants.LKQL_ID,
                                    context.getEnv().getPublicTruffleFile(LKQLFile))
                            .build();
            final CallTarget callTarget = context.getEnv().parseInternal(source);

            // Get the namespace of the LKQL config file to extract rule configuration
            final LKQLNamespace namespace = (LKQLNamespace) callTarget.call();
            final DynamicObjectLibrary objectLibrary =
                    DynamicObjectLibrary.getFactory().create(namespace);

            // Get the rules
            final Object allRulesSource = objectLibrary.getOrDefault(namespace, "rules", null);
            if (allRulesSource instanceof LKQLObject allRulesObject) {
                processRulesObject(allRulesObject, allRules, aliases, args);
            } else {
                throw LKQLRuntimeException.fromMessage(
                        "LKQL config file must define a 'rules' top level object value");
            }

            // Get the Ada rules (not mandatory)
            final Object adaRulesSource = objectLibrary.getOrDefault(namespace, "ada_rules", null);
            if (adaRulesSource instanceof LKQLObject adaRulesObject) {
                processRulesObject(adaRulesObject, adaRules, aliases, args);
            }

            // Get the SPARK rules (not mandatory)
            final Object sparkRulesSource =
                    objectLibrary.getOrDefault(namespace, "spark_rules", null);
            if (sparkRulesSource instanceof LKQLObject sparkRulesObject) {
                processRulesObject(sparkRulesObject, sparkRules, aliases, args);
            }

            // Return the result
            return new LKQLConfigFileResult(allRules, adaRules, sparkRules, aliases, args);
        } catch (IOException e) {
            throw LKQLRuntimeException.fromMessage(e.getMessage());
        }
    }

    /**
     * Process a rules object.
     *
     * @param rulesObject The rules object.
     * @param rules The list containing all parsed rules.
     * @param aliases The map containing all parsed aliases.
     * @param args The map containing all parsed arguments.
     */
    private static void processRulesObject(
            final LKQLObject rulesObject,
            final List<String> rules,
            final Map<String, String> aliases,
            final Map<String, Map<String, Object>> args) {
        // For each rule in the rules object explore its arguments
        for (Object ruleNameObject : rulesObject.keysUncached()) {
            // Get the rule arguments and check that it's an indexable value
            final String ruleName = (String) ruleNameObject;
            final Object ruleArgs = rulesObject.getUncached(ruleName);
            if (!LKQLTypeSystemGen.isIndexable(ruleArgs)) {
                throw LKQLRuntimeException.fromMessage("Rule arguments must be an indexable value");
            }

            // For each rule argument check its type and parse it
            final Object[] ruleArgsContent = LKQLTypeSystemGen.asIndexable(ruleArgs).getContent();
            if (ruleArgsContent.length == 0) {
                rules.add(ruleName);
            } else {
                for (Object ruleArg : ruleArgsContent) {
                    if (!LKQLTypeSystemGen.isLKQLObject(ruleArg)) {
                        throw LKQLRuntimeException.fromMessage(
                                "Rule argument must be an object value");
                    }
                    processArgObject(
                            ruleName,
                            LKQLTypeSystemGen.asLKQLObject(ruleArg),
                            rules,
                            aliases,
                            args);
                }
            }
        }
    }

    /**
     * Process an argument object.
     *
     * @param ruleName The name of the rule.
     * @param argObject The argument object.
     * @param rules The list containing all parsed rules.
     * @param aliases The map containing all parsed aliases.
     * @param args The map containing all parsed arguments.
     */
    private static void processArgObject(
            final String ruleName,
            final LKQLObject argObject,
            final List<String> rules,
            final Map<String, String> aliases,
            final Map<String, Map<String, Object>> args) {
        // If the arg has an alias get the name of it and add it in the alias list
        final String realName;
        final Object aliasName = argObject.getUncached(Constants.ALIAS_NAME_SYMBOL);
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
        for (Object argNameObject : argObject.keysUncached()) {
            final String argName = (String) argNameObject;

            // If the argument name is "alias_symbol" continue the exploration
            if (argName.equals(Constants.ALIAS_NAME_SYMBOL)) {
                continue;
            }

            // Put the argument in the map
            Object argValue = argObject.getUncached(argName);
            Map<String, Object> ruleArgs = args.getOrDefault(realName, new HashMap<>());
            ruleArgs.put(argName, argValue);
            args.put(realName, ruleArgs);
        }
    }
}
